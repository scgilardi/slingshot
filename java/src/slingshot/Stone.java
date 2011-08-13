package slingshot;

public class Stone extends RuntimeException {

    public final String messagePrefix;
    public final Object object;
    public final Object context;

    public Stone(String _messagePrefix, Object _object, Object _context) {
        messagePrefix = _messagePrefix;
        object = _object;
        context = _context;
    }

    public String getMessage() {
        Object messageDetails;
        try {
            clojure.lang.Var prStr = clojure.lang.RT.var("clojure.core", "pr-str");
            messageDetails = prStr.invoke(object);
        }
        catch (Exception e) {
            messageDetails = object;
        }
        return messagePrefix + " " + messageDetails;
    }
}
