package openjoe.smart.sso.server.enums;

import openjoe.smart.sso.server.stage.core.IErrorCode;

/**
 * 错误码枚举
 */
public enum ErrorCodeEnum implements IErrorCode {

    E1001(1001, "密码不能为空"),
    E1002(1002, "密码加密错误"),
    E1003(1003, "应用编码已存在"),
    E1004(1004, "登录名已存在");

    private Integer code;
    private String desc;

    ErrorCodeEnum(Integer code, String desc) {
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