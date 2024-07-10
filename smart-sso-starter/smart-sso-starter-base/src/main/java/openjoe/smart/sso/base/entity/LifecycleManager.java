package openjoe.smart.sso.base.entity;

/**
 * 对象生命周期管理器
 *
 * @author Joe
 */
public interface LifecycleManager<T> {

    /**
     * 创建
     *
     * @param key
     * @param value
     */
    void create(String key, T value);

    /**
     * 获取
     *
     * @param key
     * @return
     */
    T get(String key);

    /**
     * 删除
     *
     * @param key
     */
    void remove(String key);
}
