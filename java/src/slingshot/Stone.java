package slingshot;

public class Stone extends RuntimeException {

    public final Object context;

    public Stone(String message, Throwable cause, Object _context) {
        super (message, cause);
        context = _context;
    }
}
