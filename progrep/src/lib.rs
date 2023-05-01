//! Facilitates making progress messages and data available

use ljumvall_error::define_error;
use rand_map::RandMap;
use std::time::{Duration, SystemTime};

define_error! {
    /// `String` as an error type, displays as `ProgressReporter | `<string>.
    "ProgressReporter | {0}"
}

/// The argument to a [subscriber's](#subscribers) callback function.
///
#[derive(Clone, Debug)]
pub enum ProgressData {
    /// The `value` of [`dial`](#dials) has been updated. The `value` never
    /// includes the `fraction`, even if the current fraction subject is in
    /// fact `dial`.
    ///
    /// `message` is an optional message.
    ///
    DialUpdated {
        dial: ProgressSubject,
        value: f64,
        message: Option<String>,
    },

    /// A `message` concering `subject`.
    ///
    Message {
        subject: ProgressSubject,
        message: String,
    },

    /// Periodic update. The vector elements are
    /// (dial handle, dial name, dial value, fraction value).
    ///
    Tick(Vec<(ProgressSubject, String, f64, Option<f64>)>),
}

/// A progress reporting struct that facilitates messaging concerning a number
/// of [subjects](#subjects).
///
/// Also manages the values of a number of [dials](#dials), a subset of the
/// subjects, with an optional [fraction](#fraction) for one of the dials.
///
/// The intended use case is for a library crate that performs some lengthy
/// operation(s) to publish success messages and data, and one or more crates
/// to subscribe to (some selection of) that information.
///
/// ## Subjects
///
/// Progress reporting refers to a number of <b>subjects</b>. A subject has a
/// name and is identified by a [handle](struct.ProgressSubject.html).
///
/// All handles can be used to [send progress messages](#method.publish) to
/// [subscribers](#method.subscribe).
///
/// ## Dials
///
/// A subject may be a <b>dial</b> that keeps a value that can be
/// [incremented or decremented](#method.change_dial) by the reporting crate.
/// A [subscribing](#method.subscribe) crate may get a periodic update of the
/// values.
///
/// ## Fraction
///
/// One dial at a time may show [fractional progress](#method.set_fraction)
/// between changes of the dial value.
///
/// ## Reporter
///
/// A <b>reporter</b> is an item that wishes to report progress. It should
/// instantiate a `ProgressReporter` and use these methods to report progress.
///
/// ## Subscribers
///
/// A <b>subscriber</b> [subscribes](#method.subscribe) to the reports
/// through a registered callback with a [`ProgressData`
/// ](enum.ProgressData.html) parameter.
///
/// ## Example
///
/// As stated above, the intended use pattern is that a library crate
/// instantiates a `ProgressReporter` and the library crate's users subscribe
/// to reports. But as this example shows the reporter and subscriber may
/// share crate.
/// ```
/// use std::cell::RefCell;
/// use std::rc::Rc;
/// use ljumvall_progrep::{ProgressData, ProgressReporter};
///
/// fn do_something_expensive(millis: u64) {
///     std::thread::sleep(std::time::Duration::from_millis(millis));
/// }
///
/// // Any variable that the callback closure acesses should be an
/// // Rc<RefCell> (or Arc<Mutex> if multithreaded).
/// let log = Rc::new(RefCell::new(String::new()));
/// let (messages, dials, mut progress) = ProgressReporter::new(&["msg"], &["dia"]);
/// let message_subject = messages[0];
/// let dial_subject = dials[0];
/// let callback = progress.subscribe(
///     |progress_data| {
///         match progress_data {
///             ProgressData::DialUpdated { dial, value, message } => {
///                 *log.borrow_mut() += &format!(
///                     "dial: {:?}, value: {}, message: {:?}\n",
///                     dial,
///                     value,
///                     message,
///                 );
///             }
///             ProgressData::Message { subject, message } => {
///                 *log.borrow_mut() += &format!("message: {}\n", message);
///             }
///             ProgressData::Tick(dials) => {
///                 *log.borrow_mut() += &format!("tick {:?}\n", dials);
///             }
///         }
///     },
///     true,
///     &["dia", "msg"],
///     None,
/// );
///
/// progress.publish(message_subject, "a message");
/// assert_eq!(*log.borrow(), "message: a message\n");
/// *log.borrow_mut() = String::new();
/// progress.tick();
/// // first tick() always generates a callback
/// assert_eq!(*log.borrow(), "tick [(ProgressSubject(1), \"dia\", 0.0, None)]\n");
/// *log.borrow_mut() = String::new();
/// progress.change_dial(dial_subject, 42.0, None);
/// assert_eq!(
///     *log.borrow(),
///     "dial: ProgressSubject(1), value: 42, message: None\n",
/// );
/// progress.tick();
/// // no callback, less than a second passed
/// assert_eq!(
///     *log.borrow(),
///     "dial: ProgressSubject(1), value: 42, message: None\n",
/// );
/// do_something_expensive(400);
/// progress.set_fraction(dial_subject, 0.2);
/// // still no callback
/// assert_eq!(
///     *log.borrow(),
///     "dial: ProgressSubject(1), value: 42, message: None\n",
/// );
/// do_something_expensive(800);
/// progress.set_fraction(dial_subject, 0.6);
/// // tick triggered, new data
/// let logs = log.borrow().split("\n").map(|s| s.to_string()).collect::<Vec<_>>();
/// assert_eq!(logs[1], "tick [(ProgressSubject(1), \"dia\", 42.0, Some(0.6))]");
/// do_something_expensive(1000);
/// *log.borrow_mut() = String::new();
/// progress.change_dial(dial_subject, 1.0, Some("incremented!"));
/// let logs =
///     log.borrow().split("\n").map(|s| s.to_string()).collect::<Vec<_>>();
/// // message sent ...
/// assert_eq!(
///     logs[0],
///     "dial: ProgressSubject(1), value: 43, message: Some(\"incremented!\")",
/// );
/// // ... and tick triggered
/// assert_eq!(
///     logs[1],
///     "tick [(ProgressSubject(1), \"dia\", 43.0, None)]",
/// );
/// ```
#[derive(Debug)]
pub struct ProgressReporter<'a> {
    subjects: Vec<SubjectData>,
    subscriptions: RandMap<SubscriptionData<'a>>,
    fraction: Option<Fraction>,
    period: Duration,
    next_tick: SystemTime,
}

impl<'a> ProgressReporter<'a> {
    /// Creation.
    ///
    /// `messages` are the names of a number of message-only [subjects
    /// ](#subjects).
    ///
    /// `dials` are the names of a number of [dials](#dials).
    ///
    /// If a `dial` coincides with a `message` only the `dial` is noted.
    ///
    /// Returns handles to the messages and dials in the order given.
    ///
    pub fn new(
        messages: &[&str],
        dials: &[&str],
    ) -> (Vec<ProgressSubject>, Vec<ProgressSubject>, Self) {
        let messages = messages
            .iter()
            .filter(|m| !dials.contains(m))
            .collect::<Vec<_>>();
        let mut message_handles = Vec::new();
        let mut dial_handles = Vec::new();
        let mut subjects = Vec::new();
        for (ix, subject) in messages.iter().enumerate() {
            message_handles.push(ProgressSubject(ix));
            subjects.push(SubjectData {
                name: subject.to_string(),
                value: None,
            });
        }
        for (ix, subject) in dials.iter().enumerate() {
            dial_handles.push(ProgressSubject(ix + message_handles.len()));
            subjects.push(SubjectData {
                name: subject.to_string(),
                value: Some(0.0),
            });
        }
        (
            message_handles,
            dial_handles,
            Self {
                subjects,
                subscriptions: RandMap::new(),
                fraction: None,
                period: Duration::from_secs(1),
                next_tick: SystemTime::now(),
            },
        )
    }

    /// Used by a [reporter](#reporter) to change the value of the dial `dial`
    /// by `amount`.
    ///
    /// Also removes the current [fraction](#fraction).
    ///
    /// Calls each [subscribed](#method.subscribe) callback that selected
    /// `dial` with [`DialUpdated`
    /// ](enum.ProgressData.html#variant.DialUpdated).
    ///
    /// calls [`tick()`](#method.tick).
    ///
    /// ## Error
    ///
    /// Returns an error if `dial` is not in fact a dial.
    ///
    pub fn change_dial(
        &mut self,
        dial: ProgressSubject,
        amount: f64,
        message: Option<&str>,
    ) -> Result<(), Error> {
        *(self.dial_mut(dial)?.value.as_mut().unwrap()) += amount;
        self.fraction = None;
        let progress_data = ProgressData::DialUpdated {
            dial,
            value: self.subjects.get(dial.0).unwrap().value.unwrap(),
            message: message.map(|m| m.to_string()),
        };
        for (_, subscription) in self.subscriptions.iter_mut() {
            if subscription.messages.contains(&dial) {
                (subscription.callback)(&progress_data);
            }
        }
        self.tick();
        Ok(())
    }

    /// Used by a [reporter](#reporter) to clear the values of the dials in
    /// `dials` or, if `dials` is empty, all dials.
    ///
    /// ## Error
    ///
    /// Returns an error if a subject in `dials` is not in fact a dial.
    ///
    pub fn clear(&mut self, dials: &[ProgressSubject]) -> Result<(), Error> {
        for (ix, subject) in self.subjects.iter_mut().enumerate() {
            if !subject.is_dial() {
                if dials.contains(&ProgressSubject(ix)) {
                    return Err(Error(format!(
                        "invalid dial handle: {}",
                        ix
                    )));
                }
                continue;
            }
            if dials.is_empty() || dials.contains(&ProgressSubject(ix)) {
                subject.value = Some(0.0);
                if let Some(frac) = &self.fraction {
                    if frac.dial.0 == ix {
                        self.fraction = None;
                    }
                }
            }
        }
        Ok(())
    }

    /// Used by a [subscriber](#subscribers) to get the value of a dial.
    ///
    /// Returns a pair, the `dial`s value and optionally fraction (if `dial`
    /// is the current fraction).
    ///
    /// ## Error
    ///
    /// Returns an error if `dial` is not in fact a dial.
    ///
    pub fn dial(
        &self,
        dial: ProgressSubject,
    ) -> Result<(f64, Option<f64>), Error> {
        Ok((
            self.dial_brw(dial)?.value.unwrap(),
            self.fraction.as_ref().and_then(|f| {
                if f.dial == dial {
                    Some(f.fraction)
                } else {
                    None
                }
            }),
        ))
    }

    /// Used by a [subscriber](#subscribers) to get the value of all dials.
    ///
    /// Returns a vector of tuples: (handle, name, value, optional fraction).
    ///
    pub fn dials(&self) -> Vec<(ProgressSubject, String, f64, Option<f64>)> {
        self.subjects
            .iter()
            .enumerate()
            .filter(|(_, s)| s.value.is_some())
            .map(|(ix, s)| {
                let handle = ProgressSubject(ix);
                let (value, fraction) = self.dial(handle).unwrap();
                (handle, s.name.clone(), value, fraction)
            })
            .collect()
    }

    /// Is `subject` a dial?
    ///
    /// Silently returns `false` if `subject` is not in fact a subject.
    ///
    pub fn is_dial(&self, subject: ProgressSubject) -> bool {
        self.subjects
            .get(subject.0)
            .map(|s| s.value.is_some())
            .unwrap_or(false)
    }

    /// Used by a [reporter](#reporter) to send a message to interested
    /// [subscribers](#subscribers).
    ///
    pub fn publish(&mut self, subject: ProgressSubject, message: &str) {
        for (_, subscription) in self.subscriptions.iter_mut() {
            if subscription.messages.contains(&subject) {
                (subscription.callback)(&ProgressData::Message {
                    subject,
                    message: message.to_string(),
                });
            }
        }
    }

    /// Used by a [reporter](#reporter) to set the current [fraction
    /// ](#fraction) dial to `dial` and the fraction value to `fract`, which
    /// should be in the range `0..1`, but there is no check.
    ///
    /// Calls [`tick()`](#method.tick), but does not call any callback
    /// directly.
    ///
    /// ## Error
    ///
    /// Returns an error if `dial` is not in fact a dial.
    ///
    pub fn set_fraction(
        &mut self,
        dial: ProgressSubject,
        fract: f64,
    ) -> Result<(), Error> {
        self.dial_mut(dial)?;
        self.fraction = Some(Fraction {
            dial,
            fraction: fract,
        });
        self.tick();
        Ok(())
    }

    /// Return the [`ProgressSubject`](struct.ProgressSubject.html) handle to
    /// `subject`.
    ///
    /// ## Error
    ///
    /// Returns an error if `subject` is not in fact a subject.
    ///
    pub fn subject(&self, subject: &str) -> Result<ProgressSubject, Error> {
        if let Some((ix, _)) = self
            .subjects
            .iter()
            .enumerate()
            .find(|(_, data)| data.name == subject)
        {
            Ok(ProgressSubject(ix))
        } else {
            Err(Error(format!("invalid subject name: {}", subject)))
        }
    }

    /// Connect a [subscriber](#subscribers) to `self` and filter what
    /// [reports](#reporter) that trigger `callback()`.
    ///
    /// Returns a handle to the subscription. This is to enable termination of
    /// the subscription.
    ///
    /// If `use_dials` is `false`, the [`tick()`](#method.tick) method does
    /// not call `callback()`.
    ///
    /// If a subject is missing from `messages`, the [`change_dial()`
    /// ](#method.change_dial) and [`publish()`](#method.publish) methods do
    /// not call `callback()` for that subject.
    ///
    /// if `old_subscription` is `Some`, that subscription is replaced by the
    /// new one. The returned handle is not equal to `old_subscription`.
    ///
    /// if `use_dials` is `false` and `messages` is empty, the callback is
    /// ignored and an error returned. This is a not very elegant way to
    /// unsubscribe.
    ///
    /// ## Error
    ///
    /// Returns an error if any of the subjects in `messages` is not in fact a
    /// valid subject.
    ///
    /// Returns an error if `use_dials` is `false` and `messages` is empty.
    ///
    pub fn subscribe(
        &mut self,
        callback: impl Fn(&ProgressData) + 'a,
        use_dials: bool,
        messages: &[&str],
        old_subscription: Option<ProgressSubscription>,
    ) -> Result<ProgressSubscription, Error> {
        let mut message_subjects = Vec::new();
        for message in messages {
            if let Some((ix, _)) = self
                .subjects
                .iter()
                .enumerate()
                .find(|(_, sb)| sb.name == *message)
            {
                message_subjects.push(ProgressSubject(ix));
            } else {
                return Err(Error(format!(
                    "invalid message name: {}",
                    message
                )));
            }
        }
        old_subscription.map(|old| self.subscriptions.remove(old.0.into()));
        if !use_dials && messages.is_empty() {
            return Err(Error("neither dials or messages given".to_string()));
        }
        Ok(ProgressSubscription(
            self.subscriptions
                .insert(SubscriptionData {
                    callback: Box::new(callback),
                    use_dials,
                    messages: message_subjects,
                })
                .as_u64(),
        ))
    }

    /// May be used by a [reporter](#reporter) to inform `self` that it has
    /// done some work that possibly should generate a [tick
    /// ](enum.ProgressData.html#variant.Tick) to the [subscribers
    /// ](#subscribers). Since both [`change_dial()`
    /// ](#method.change_dial) and [`set_fraction()`](#method.set_fraction) do
    /// call this method, the reporter normally does not have to.
    ///
    /// If more than a second has passed, a tick is sent to callbacks that
    /// have [subscribed](#method.subscribe) to `use_dials` and `true` is
    /// returned. Otherwise `false`.
    ///
    pub fn tick(&mut self) -> bool {
        let mut tick = false;
        loop {
            if self.next_tick > SystemTime::now() {
                break;
            }
            tick = true;
            self.next_tick += self.period;
        }
        if tick {
            let dials = self.dials();
            for (_, subscription) in self.subscriptions.iter_mut() {
                if subscription.use_dials {
                    (subscription.callback)(&ProgressData::Tick(
                        dials.clone(),
                    ));
                }
            }
        }
        tick
    }

    // --- private -----------------------------------------------------------

    fn dial_brw(
        &'a self,
        dial: ProgressSubject,
    ) -> Result<&'a SubjectData, Error> {
        if let Some(data) = self.subjects.get(dial.0) {
            if data.value.is_some() {
                return Ok(data);
            }
        }
        Err(Error(format!("invalid dial handle: {}", dial.0)))
    }

    fn dial_mut(
        &mut self,
        dial: ProgressSubject,
    ) -> Result<&mut SubjectData, Error> {
        if let Some(data) = self.subjects.get_mut(dial.0) {
            if data.value.is_some() {
                return Ok(data);
            }
        }
        Err(Error(format!("invalid dial handle: {}", dial.0)))
    }
}

/// An opaque handle to a progress reporting subject
///
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProgressSubject(usize);

/// An opaque handle to a progress subscription
///
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProgressSubscription(u64);

// --- private ---------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq)]
struct Fraction {
    dial: ProgressSubject,
    fraction: f64,
}

struct SubscriptionData<'a> {
    callback: Box<dyn Fn(&ProgressData) + 'a>,
    use_dials: bool,
    messages: Vec<ProgressSubject>,
}

impl std::fmt::Debug for SubscriptionData<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SubscriptionData")
            .field("use_dials", &self.use_dials)
            .field("messages", &self.messages)
            .finish_non_exhaustive()
    }
}

// Data for a progress reporting subject
//
#[derive(Debug)]
struct SubjectData {
    name: String,
    value: Option<f64>,
}

impl SubjectData {
    fn is_dial(&self) -> bool {
        self.value.is_some()
    }
}

#[cfg(test)]
#[allow(unused_assignments)]
#[allow(unused_mut)]
#[allow(unused_variables)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::thread::sleep;

    speculate2::speculate! {
        describe "struct ProgressReporter" {
            before {
                let log = Rc::new(RefCell::new(String::new()));
                let log2 = Rc::new(RefCell::new(String::new()));
                let subs: Rc<RefCell<ProgressSubscription>>;
                let (m1, m2, d1, d2, mut pr) = {
                    let (ms, ds, pr) =
                        ProgressReporter::new(&["m1", "m2"], &["d1", "d2"]);
                    (ms[0], ms[1], ds[0], ds[1], pr)
                };
                subs = Rc::new(RefCell::new(pr.subscribe(
                    |pd| *log.borrow_mut() += &format!("{:?}\n", pd),
                    true,
                    &["m1", "m2", "d1", "d2"],
                    None,
                ).unwrap()));
            }

            context "new()" {
                before {
                    let (ms, ds, constructed) = ProgressReporter::new(
                        &["m1", "d2", "m2"],
                        &["d1", "d2"],
                    );
                }

                it "removes messages that are also dials" {
                    assert_eq!(constructed.subjects.len(), 4);
                }

                it "gives no value to messages" {
                    assert_eq!(constructed.subjects[0].name, "m1");
                    assert_eq!(constructed.subjects[0].value, None);
                    assert_eq!(constructed.subjects[1].name, "m2");
                    assert_eq!(constructed.subjects[1].value, None);
                }

                it "zeroes value for dials" {
                    assert_eq!(constructed.subjects[2].name, "d1");
                    assert_eq!(constructed.subjects[2].value, Some(0.0));
                    assert_eq!(constructed.subjects[3].name, "d2");
                    assert_eq!(constructed.subjects[3].value, Some(0.0));
                }

                it "has no subscriptions" {
                    assert!(
                        constructed.subscriptions.as_hash_map().is_empty(),
                    );
                }

                it "has no fraction" {
                    assert!(
                        constructed.fraction.is_none(),
                    );
                }

                it "is set to tick immediately" {
                    assert!(constructed.next_tick <= SystemTime::now());
                }
            }

            context "change_dial()" {
                before {
                    pr.subjects[2].value = Some(1.0);
                    pr.subjects[3].value = Some(2.0);
                    pr.fraction = Some(Fraction { dial: d2, fraction: 0.5 });
                    let _ = pr.change_dial(d2, 1.0, Some("msg"));
                }

                it "changes the value" {
                    assert_eq!(pr.subjects[2].value, Some(1.0));
                    assert_eq!(pr.subjects[3].value, Some(3.0));
                }

                it "removes the current fraction" {
                    assert!(pr.fraction.is_none());
                }

                it "callback for dial if selected" {
                    let log = log.borrow();
                    assert!(log.contains(
                        "DialUpdated { \
                            dial: ProgressSubject(3), \
                            value: 3.0, \
                            message: Some(\"msg\") \
                        }",
                    ), "{}", log);
                }

                it "calls tick()" {
                    let log = log.borrow();
                    assert!(log.contains(
                        "Tick([\
                            (ProgressSubject(2), \"d1\", 1.0, None), \
                            (ProgressSubject(3), \"d2\", 3.0, None)\
                        ])",
                    ), "{}", log);
                }

                context "unsubscribe message" {
                    before {
                        log.borrow_mut().truncate(0);
                        pr.next_tick = SystemTime::now();
                        let old_subs = *subs.borrow();
                        *subs.borrow_mut() = pr.subscribe(
                            |pd| *log.borrow_mut() += &format!("{:?}\n", pd),
                            true,
                            &["m1", "m2", "d1"],
                            Some(old_subs),
                        )
                        .unwrap();
                        let _ = pr.change_dial(d2, 1.0, Some("msg"));
                    }

                    it "no callback for dial if not selected" {
                        let log = log.borrow();
                        assert!(!log.contains("DialUpdated"), "{}", log);
                    }

                    it "still calls tick()" {
                        let log = log.borrow();
                        assert!(log.contains(
                            "Tick([\
                                (ProgressSubject(2), \"d1\", 1.0, None), \
                                (ProgressSubject(3), \"d2\", 4.0, None)\
                            ])",
                        ), "{}", log);
                    }
                }

                context "unsubscribe tick" {
                    before {
                        log.borrow_mut().truncate(0);
                        pr.next_tick = SystemTime::now();
                        let old_subs = *subs.borrow();
                        *subs.borrow_mut() = pr.subscribe(
                            |pd| *log.borrow_mut() += &format!("{:?}\n", pd),
                            false,
                            &["m1", "m2", "d2"],
                            Some(old_subs),
                        )
                        .unwrap();
                        let _ = pr.change_dial(d2, 1.0, Some("msg"));
                    }

                    it "still callback for dial" {
                        let log = log.borrow();
                        assert!(log.contains(
                            "DialUpdated { \
                                dial: ProgressSubject(3), \
                                value: 4.0, \
                                message: Some(\"msg\") \
                            }",
                        ), "{}", log);
                    }

                    it "no longer calls tick()" {
                        let log = log.borrow();
                        assert!(!log.contains("Tick"), "{}", log);
                    }
                }

                it "returns error if erronous dial" {
                    assert!(pr.change_dial(m2, 1.0, Some("msg")).is_err());
                }
            }

            context "clear()" {
                before {
                    pr.subjects[2].value = Some(1.0);
                    pr.subjects[3].value = Some(2.0);
                }

                it "clears the values in dials" {
                    assert!(pr.clear(&[d2]).is_ok());
                    assert_eq!(pr.subjects[2].value, Some(1.0));
                    assert_eq!(pr.subjects[3].value, Some(0.0));
                }

                it "clears all values if dials is empty" {
                    assert!(pr.clear(&[]).is_ok());
                    assert_eq!(pr.subjects[2].value, Some(0.0));
                    assert_eq!(pr.subjects[3].value, Some(0.0));
                }

                it "returns error if no dial" {
                    assert!(pr.clear(&[d2, m1]).is_err());
                }
            }

            context "dial()" {
                before {
                    pr.subjects[2].value = Some(1.0);
                    pr.subjects[3].value = Some(2.0);
                    pr.fraction = Some(Fraction { dial: d2, fraction: 0.5 });
                }

                it "returns (value, Option<fraction>)" {
                    assert_eq!(pr.dial(d2), Ok((2.0, Some(0.5))));
                }

                it "returns error if no dial" {
                    assert!(pr.dial(m1).is_err());
                }
            }

            context "dials()" {
                before {
                    pr.subjects[2].value = Some(1.0);
                    pr.subjects[3].value = Some(2.0);
                    pr.fraction = Some(Fraction { dial: d2, fraction: 0.5 });
                }

                it "returns [(handle, name, value, Option<fraction>), ...]" {
                    let ds = pr.dials();
                    assert!(ds.contains(&(d1, "d1".to_string(), 1.0, None)));
                    assert!(ds.contains(
                        &(d2, "d2".to_string(), 2.0, Some(0.5)),
                    ));
                }
            }

            context "is_dial()" {
                it "true if indeed a dial" {
                    assert!(pr.is_dial(d1));
                    assert!(pr.is_dial(d2));
                }

                it "false if not a dial" {
                    assert!(!pr.is_dial(m1));
                    assert!(!pr.is_dial(m2));
                }

                it "false if not a subject" {
                    let no_subject = ProgressSubject(4711);
                    assert!(!pr.is_dial(no_subject));
                }
            }

            context "publish()" {
                before {
                    pr.publish(m2, "m2");
                    pr.publish(d1, "d1");
                }

                it "sends a message to subscribers" {
                    let log = log.borrow();
                    assert!(log.contains( "Message { \
                        subject: ProgressSubject(1), message: \"m2\" \
                    }"));
                    assert!(log.contains( "Message { \
                        subject: ProgressSubject(2), message: \"d1\" \
                    }"));
                }

                context "unsubscribe message" {
                    before {
                        log.borrow_mut().truncate(0);
                        let old_subs = *subs.borrow();
                        *subs.borrow_mut() = pr.subscribe(
                            |pd| *log.borrow_mut() += &format!("{:?}\n", pd),
                            true,
                            &["m1", "d1"],
                            Some(old_subs),
                        )
                        .unwrap();
                        pr.publish(m2, "m2");
                        pr.publish(d1, "d1");
                    }

                    it "no message if not selected" {
                        let log = log.borrow();
                        assert!(!log.contains("m2"));
                        assert!(log.contains( "Message { \
                            subject: ProgressSubject(2), message: \"d1\" \
                        }"));
                    }
                }
            }

            context "set_fraction()" {
                before {
                    pr.subjects[2].value = Some(1.0);
                    pr.subjects[3].value = Some(2.0);
                }

                it "sets the current fraction dial" {
                    assert!(pr.set_fraction(d2, 0.5).is_ok());
                    assert!(pr.fraction.is_some());
                    assert_eq!(
                        pr.fraction.unwrap(),
                        Fraction { dial: d2, fraction: 0.5 },
                    );
                }

                it "does not check range" {
                    assert!(pr.set_fraction(d2, 4.711).is_ok());
                    assert!(pr.fraction.is_some());
                    assert_eq!(
                        pr.fraction.unwrap(),
                        Fraction { dial: d2, fraction: 4.711 },
                    );
                }

                it "no callback for dial" {
                    assert!(pr.set_fraction(d2, 0.5).is_ok());
                    let log = log.borrow();
                    assert!(!log.contains("DialUpdated"), "{}", log);
                }

                it "calls tick()" {
                    assert!(pr.set_fraction(d2, 0.5).is_ok());
                    let log = log.borrow();
                    assert_eq!(
                        *log,
                        "Tick([\
                            (ProgressSubject(2), \"d1\", 1.0, None), \
                            (ProgressSubject(3), \"d2\", 2.0, Some(0.5))\
                        ])\n",
                    );
                }

                it "calls no tick() if not selected" {
                    let old_subs = *subs.borrow();
                    *subs.borrow_mut() = pr.subscribe(
                        |pd| *log.borrow_mut() += &format!("{:?}\n", pd),
                        false,
                        &["m1", "m2", "d1", "d2"],
                        Some(old_subs),
                    )
                    .unwrap();
                    assert!(pr.set_fraction(d2, 0.5).is_ok());
                    assert_eq!(*log.borrow(), "");
                }

                it "returns error if erronous dial" {
                    assert!(pr.set_fraction(m2, 0.5).is_err());
                }
            }

            context "subject()" {
                it "returns the handle" {
                    assert!(pr.subject("m2").is_ok());
                    assert_eq!(pr.subject("m2").unwrap(), m2);
                }

                it "returns error if erronous subject" {
                    assert!(pr.subject("foo").is_err());
                }
            }

            context "subscribe()" {
                it "subscribing once implicit" {}
                it "returns handle implicit" {}
                it "selecting callbacks implicit" {}
                it "resubscribing implicit" {}

                it "resubscribing gives new handle" {
                    let old_subs = *subs.borrow();
                    *subs.borrow_mut() = pr.subscribe(
                        |pd| *log.borrow_mut() += &format!("{:?}\n", pd),
                        false,
                        &["m1", "m2", "d1", "d2"],
                        Some(old_subs),
                    )
                    .unwrap();
                    assert_ne!(*subs.borrow(), old_subs);
                }

                context "subscribing twice" {
                    before {
                        let subs2 = pr.subscribe(
                            |pd| *log2.borrow_mut() += &format!("{:?}\n", pd),
                            true,
                            &["m1", "d1"],
                            None,
                        )
                        .unwrap();
                    }

                    it "uses both callbacks for messages" {
                        pr.publish(d1, "d1");
                        assert!(log.borrow().contains("Message { \
                            subject: ProgressSubject(2), message: \"d1\" \
                        }"));
                        assert!(log2.borrow().contains("Message { \
                            subject: ProgressSubject(2), message: \"d1\" \
                        }"));
                    }

                    it "uses both callbacks for ticks" {
                        assert!(pr.change_dial(d1, 42.0, None).is_ok());
                        assert!(log.borrow().contains("Tick"));
                        assert!(log2.borrow().contains("Tick"));
                    }

                    it "selects differently" {
                        pr.publish(d2, "d2");
                        assert!(log.borrow().contains("Message { \
                            subject: ProgressSubject(3), message: \"d2\" \
                        }"));
                        assert!(!log2.borrow().contains("Message"));
                    }
                }

                context "unsubscribing" {
                    before {
                        let result = pr.subscribe(
                            |pd| {},
                            false,
                            &[],
                            Some(*subs.borrow()),
                        );
                    }

                    it "unsubscribes" {
                        assert!(pr.subscriptions.as_hash_map().is_empty());
                    }

                    it "returns error" {
                        assert!(result.is_err());
                    }
                }
            }

            context "tick()" {
                it "calls callbacks implicit" {}

                it "waits one second" {
                    pr.tick();
                    let first = (*log.borrow()).clone();
                    assert_eq!(
                        first,
                        "Tick([\
                            (ProgressSubject(2), \"d1\", 0.0, None), \
                            (ProgressSubject(3), \"d2\", 0.0, None)\
                        ])\n",
                    );
                    pr.tick();
                    assert_eq!(*log.borrow(), first);
                    sleep(Duration::from_millis(990));
                    pr.tick();
                    assert_eq!(*log.borrow(), first);
                    sleep(Duration::from_millis(20));
                    pr.tick();
                    assert_ne!(*log.borrow(), first);
                    assert_eq!((*log.borrow()).lines().count(), 2);
                }

                it "does not pile up ticks" {
                    sleep(Duration::from_millis(30));
                    pr.tick();
                    assert!(pr.next_tick >= SystemTime::now());
                }
            }
        }
    }
}
