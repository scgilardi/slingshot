package slingshot;

public class Stone extends RuntimeException {

    public final Object data;

    public Stone(Object obj) {
        data = obj;
    }
}
