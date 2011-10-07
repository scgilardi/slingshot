(ns slingshot.core
  (:use [slingshot.hooks :only [*throw-hook*]]
        [slingshot.support :only [make-stack-trace partition-body
                                  transform-catch validate-try+-form]]))

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
     `(*throw-hook*
       (let [env# (zipmap '~(keys &env) [~@(keys &env)])]
         {:object ~object
          :message ~message
          :cause (-> (env# '~'&throw-context) meta :throwable)
          :stack-trace (make-stack-trace)
          :environment (dissoc env# '~'&throw-context)})))
  ([object]
     `(throw+ ~object "Object thrown by throw+"))
  ([] `(throw (-> ~'&throw-context meta :throwable))))

(defmacro try+
  "Like the try special form, but with enhanced catch clauses:
    - specify objects to catch by classname, predicate, or
      selector form;
    - destructure the caught object;
    - access the values of the locals visible at the throw site via
      the &throw-context hidden argument.

  A selector form is a form containing one or more instances of % to
  be replaced by the thrown object. If it evaluates to truthy, the
  object is caught.

  The classname and predicate selectors are shorthand for these
  selector forms:

    <classname> => (instance? <classname> %)
    <predicate> => (<predicate> %)

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

  To throw a non-Throwable object, throw+ wraps it with an Throwable
  object of class Stone. That Stone may in turn end up wrapped by
  other exceptions (e.g., instances of RuntimeException or
  java.util.concurrent.ExecutionException). try+ \"sees through\" all
  such wrappers to find the object wrapped by the first instance of
  Stone in the outermost wrapper's cause chain. If needed, the
  outermost wrapper is available within a catch clause a via
  the :wrapper key in &throw-context. Any nested wrappers are
  accessible via its cause chain.

  When throw+ throws a non-Throwable object from within a try+ catch
  clause, the outermost wrapper of the caught object being processed
  is captured as the \"cause\" of the new throw+.

  See also throw+"
  [& body]
  (let [[exprs catch-clauses finally-clauses sentinel] (partition-body body)]
    (validate-try+-form exprs catch-clauses finally-clauses sentinel)
    `(try
       ~@exprs
       ~@(when catch-clauses
           [(transform-catch catch-clauses '(throw+))])
       ~@finally-clauses)))
