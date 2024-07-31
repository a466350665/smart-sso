package openjoe.smart.sso.server.stage.core;

/**
 * 消息基础接口
 *
 * @author Joe
 */
public interface IMessage {

    /**
     * 键
     */
    String getKey();

    default String getMessage(Object... args) {
        return Message.get(getKey(), args);
    }
}