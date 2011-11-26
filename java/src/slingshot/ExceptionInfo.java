package slingshot;

import clojure.lang.IPersistentMap;

public class ExceptionInfo extends RuntimeException {

    final IPersistentMap data;

    public ExceptionInfo(String message, IPersistentMap data, Throwable cause) {
        super(message, cause);
        this.data = data;
    }

    public IPersistentMap getData () {
        return data;
    }
}
