package openjoe.smart.sso.server.enums;


import openjoe.smart.stage.core.enums.ErrorCode;

/**
 * 错误码枚举
 */
public enum ErrorCodeEnum implements ErrorCode {

    E010001("010001", "密码不能为空"),
    E010002("010002", "密码加密错误"),
    E010003("010003", "应用编码已存在"),
    E010004("010004", "登录名已存在");

    private String code;
    private String desc;

    ErrorCodeEnum(String code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    @Override
    public String getCode() {
        return code;
    }

    @Override
    public String getDesc() {
        return desc;
    }
}