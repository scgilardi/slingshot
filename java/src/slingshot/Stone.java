package slingshot;

public class Stone extends RuntimeException {

    private final Object _data;

    public Stone(Object data) {
        _data = data;
    }

    public Object data() {
        return _data;
    }
}
