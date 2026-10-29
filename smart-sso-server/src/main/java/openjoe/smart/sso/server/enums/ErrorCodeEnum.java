package openjoe.smart.sso.server.enums;


import openjoe.smart.stage.core.enums.ErrorCode;

/**
 * 错误码枚举
 */
public enum ErrorCodeEnum implements ErrorCode {

    E010001("010001", "密码不能为空"),
    E010002("010002", "密码加密错误"),
    E010003("010003", "应用编码已存在"),
    E010004("010004", "登录名已存在"),
    E010005("010005", "权限类型有误"),
    E010006("010006", "权限编码格式有误"),
    E010007("010007", "菜单地址不能为空"),
    E010008("010008", "功能权限不能设置菜单地址");

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
