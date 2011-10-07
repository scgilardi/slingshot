package slingshot;

public class Stone extends RuntimeException {

    final Object context;

    public Stone(String message, Throwable cause, StackTraceElement[] stackTrace,
                 Object context) {
        super (message, cause);
        setStackTrace(stackTrace);
        this.context = context;
    }

    public Object getContext () {
        return context;
    }
}
