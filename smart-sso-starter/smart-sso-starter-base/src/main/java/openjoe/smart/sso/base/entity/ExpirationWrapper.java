package openjoe.smart.sso.base.entity;

/**
 * 含时效功能的对象包装器
 *
 * @param <T>
 * @author Joe
 */
public class ExpirationWrapper<T> {

    private T object;

    /**
     * 超时时间，单位（秒）
     */
    private int expiresIn;

    /**
     * 过期时间
     */
    private Long expired;

    public ExpirationWrapper() {
        super();
    }

    public ExpirationWrapper(T object, int expiresIn) {
        super();
        this.object = object;
        this.expiresIn = expiresIn;
        this.expired = System.currentTimeMillis() + expiresIn * 1000;
    }

    public T getObject() {
        return object;
    }

    public void setObject(T object) {
        this.object = object;
    }

    public int getExpiresIn() {
        return expiresIn;
    }

    public void setExpiresIn(int expiresIn) {
        this.expiresIn = expiresIn;
    }

    public Long getExpired() {
        return expired;
    }

    public void setExpired(Long expired) {
        this.expired = expired;
    }

    /**
     * 校验是否过期，true为已过期，false未未过期
     *
     * @return
     */
    public boolean checkExpired() {
        return System.currentTimeMillis() > getExpired();
    }
}