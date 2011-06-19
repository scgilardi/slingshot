package slingshot;

public class Stone extends RuntimeException {

    private final Object _data;

    public Stone(Object data) {
        super();
        _data = data;
    }

    public Object data() {
        return _data;
    }
}
