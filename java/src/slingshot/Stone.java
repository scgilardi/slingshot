package slingshot;

public class Stone extends RuntimeException {

    private final Object _map;

    public Stone(Object map) {
        super();
        _map = map;
    }

    public Object map() {
        return _map;
    }
}
