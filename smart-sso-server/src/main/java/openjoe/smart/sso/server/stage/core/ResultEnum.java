package openjoe.smart.sso.server.stage.core;

/**
 * @author Joe
 */
public enum ResultEnum implements IErrorCode {

    SUCCESS(1, "成功"),
    VALIDATION_ERROR(9998, "参数校验错误"),
    ERROR(9999, "未知错误");

    private Integer code;
    private String desc;

    ResultEnum(Integer code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    @Override
    public Integer getCode() {
        return code;
    }

    @Override
    public String getDesc() {
        return desc;
    }
}