package slingshot;

public class Stone extends RuntimeException {

    final Object context;

    public Stone(String message, Throwable cause, Object context) {
        super (message, cause);
        this.context = context;
    }

    public Object getContext () {
        return context;
    }
}
