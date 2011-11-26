package slingshot;

import clojure.lang.IPersistentMap;

public class ExceptionInfo extends RuntimeException {

    final IPersistentMap data;

    public ExceptionInfo(String message, IPersistentMap data,
                         StackTraceElement[] stackTrace, Throwable cause) {
        super(message, cause);
        setStackTrace(stackTrace);
        this.data = data;
    }

    public IPersistentMap getData () {
        return data;
    }
}
