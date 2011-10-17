(ns slingshot.core
  (:use [slingshot.support :only [env-map parse-try rethrow stack-trace
                                  throw-context transform]]))

(defmacro try+
  "Like the try special form, but with enhanced catch clauses:
    - specify objects to catch by class name, key-value pair,
      predicate, or selector form;
    - destructure the caught object;
    - access the values of the locals visible at the throw site via
      the &throw-context hidden argument.

  A selector form is a form containing one or more instances of % to
  be replaced by the thrown object. If it evaluates to truthy, the
  object is caught.

  The class name, key-value pair, and predicate selectors are shorthand
  for these selector forms:

    <class name>   => (instance? <class name> %)
    [<key> <val>] => (= (get % <key>) <val>)
    <predicate>   => (<predicate> %)

  &throw-context is a map containing:
    - for all caught objects:
      :object       the thrown object;
      :stack-trace  the stack trace;
    - for Throwable caught objects:
      :message      the message, from .getMessage;
      :cause        the cause, from .getCause;
    - for non-Throwable caught objects:
      :message      the message, from the optional argument to throw+;
      :cause        the cause, captured by throw+, see below;
      :wrapper      the outermost Throwable wrapper of the caught object,
                    see below;
      :environment  a map of locals visible at the throw+ site: symbols
                    mapped to their bound values.

  To throw a non-Throwable object, throw+ wraps it with a Throwable
  object of class Stone. That Stone may in turn end up wrapped by
  other exceptions (e.g., instances of RuntimeException or
  java.util.concurrent.ExecutionException). try+ sees through all
  such wrappers to find the object wrapped by the first instance of
  Stone in the outermost wrapper's cause chain. If needed, the
  outermost wrapper is available within a catch clause a via
  the :wrapper key in &throw-context. Any nested wrappers are
  accessible via its cause chain.

  When throw+ throws a non-Throwable object from within a try+ catch
  clause, the outermost wrapper of the caught object being processed
  is captured as the cause of the new throw+.

  See also throw+"
  [& body]
  (let [[exprs catch-clauses finally-clauses] (parse-try body)]
    `(try
       ~@exprs
       ~@(transform catch-clauses `throw+)
       ~@finally-clauses)))

(defmacro throw+
  "Like the throw special form, but can throw any object. Behaves the
  same as throw for Throwable objects. For other objects, an optional
  second argument specifies a message that is accessible in catch
  clauses within both try forms (via .getMessage on the throwable
  wrapper), and try+ forms (via the :message key in &throw-context).
  Within a try+ catch clause, throw+ with no arguments rethrows the
  caught object within its original (possibly nested) wrappers.

  See also try+"
  ([object message]
     `(throw-context ~object ~message (stack-trace) (env-map)))
  ([object]
     `(throw+ ~object "Object thrown by throw+"))
  ([]
     `(rethrow ~'&throw-context)))
