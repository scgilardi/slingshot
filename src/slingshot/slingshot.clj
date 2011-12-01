(ns slingshot.slingshot
  (:require [slingshot.support :as s]))

(defmacro try+
  "Like the try special form, but with enhanced catch clauses:

    - catch non-Throwable objects thrown by throw+ as well as
      Throwable objects thrown by throw or throw+;

    - specify objects to catch by class name, key-value pair,
      predicate, or arbitrary selector form;

    - destructure the caught object;

    - in a catch clause, access the names and values of the locals
      visible at the throw site, including the name of the enclosing
      function and its arguments (unless shadowed by nested locals).

  A selector form is a form containing one or more instances of % to
  be replaced by the thrown object. If it evaluates to truthy, the
  object is caught.

    The class name, key-value pair, and predicate selectors are
    shorthand for these selector forms:

      <class name>  => (instance? <class name> %)
      [<key> <val>] => (= (get % <key>) <val>)
      <predicate>   => (<predicate> %)

  &throw-context is a local acessible within try+ catch clauses. It is
  a map containing:
  The binding form in a try+ catch clause is not required to be a
  simple symbol. It is subject to destructuring which allows easy
  access to the contents of a thrown collection.

    - for Throwable caught objects:
      :object       the caught object;
      :message      the message, from .getMessage;
      :cause        the cause, from .getCause;
      :stack-trace  the stack trace, from .getStackTrace;
      :throwable    the caught object;

    - for non-Throwable caught objects:
      :object       the caught object;
      :message      the message, from the optional arguments to throw+;
      :cause        the cause, captured by throw+, see below;
      :stack-trace  the stack trace, captured by throw+;
      :throwable    the outermost Throwable wrapper of the caught object,
                    see below;
      :environment  a map from names to values for locals visible at
                    the throw+ site.

  To throw a non-Throwable object, throw+ wraps it in a Throwable
  context wrapper. That wrapper may in turn end up wrapped by other
  exceptions (e.g., instances of RuntimeException or
  java.util.concurrent.ExecutionException). When processing catch
  clauses, try+ sees through any such wrappers to find the thrown
  object. If needed, the outermost wrapper is available within a catch
  clause via the :throwable key in &throw-context. Any nested wrappers
  are accessible via its cause chain.
  The hidden local &throw-context is visible within try+ catch
  clauses, bound to the throw context for the caught object.

  When throw+ throws a non-Throwable object from within a try+ catch
  clause, the outermost wrapper of the caught object being processed
  is captured as the cause of the new throw+.

  See also throw+"
  [& body]
  (let [[expressions catch-clauses finally-clauses] (s/parse-try+ body)]
    `(try
       ~@expressions
       ~@(s/transform-catch catch-clauses `throw+)
       ~@finally-clauses)))

(defmacro throw+
  "Like the throw special form, but can throw any object by wrapping
  non-Throwable objects in a Throwable wrapper.

  throw+ has the same syntax and behavior as throw for Throwable
  objects. The message, cause, and stack trace are those carried by
  the Throwable.

  For non-Throwable objects, throw+ packages the object, message,
  cause, stack trace, and environment in a Throwable wrapper:

    - message: specified by an optional format string and args for
      clojure.core/format:

      - % symbols (at any nesting depth) within args represent the
        thrown object

      - the default is: \"throw+: %s\" (pr-str %)

    - cause: for a throw+ call within a try+ catch clause, the cause
      is the outermost wrapper of the caught object being processed.
      In any other case, the cause is nil;

    - stack trace: the stack trace of the current thread at the time
      of the throw+ call, starting at the function that encloses it;

    - environment: a map from names to values for locals visible at
      the throw+ call site, including the enclosing function and its
      arguments (unless shadowed by nested locals).

  Within a try+ catch clause, a throw+ call with no arguments rethrows
  the caught object within its original (possibly nested) wrappers.

  See also try+"
  ([object]
     `(throw+ ~object "throw+: %s" (pr-str ~'%)))
  ([object fmt & args]
     (let [obj (gensym)]
       `(let [~obj ~object]
          (s/throw-context ~obj
                           (format ~fmt ~@(s/replace-all {'% obj} args))
                           (s/stack-trace)
                           (dissoc (s/environment) '~obj)))))
  ([]
     `(s/rethrow)))

(defn get-throw-context
  "Returns a throw context given a Throwable t caught within an
  ordinary try form. If t or any Throwable in its cause chain is a
  context wrapper, returns the context with t assoc'd as the value
  for :throwable, else returns a new context based on t.

  See also try+"
  [t]
  (s/get-context t))

(defn get-thrown-object
  "Returns the object thrown by throw or throw+ given a Throwable
  caught within an ordinary try form."
  [t]
  (-> t get-throw-context :object))
