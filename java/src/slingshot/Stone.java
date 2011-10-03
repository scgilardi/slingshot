package slingshot;

public class Stone extends RuntimeException {

    public final Object context;
    public final clojure.lang.IFn format;

    public Stone(Object _context, clojure.lang.IFn _format) {
        context = _context;
        format = _format;
    }

    public String getMessage() {
        Object message;
        try {
            message = format.invoke(context);
        }
        catch (Exception e) {
            message = context;
        }
        return message.toString();
    }
}
