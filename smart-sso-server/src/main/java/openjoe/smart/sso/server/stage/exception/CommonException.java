package openjoe.smart.sso.server.stage.exception;

import openjoe.smart.sso.server.stage.core.ResultEnum;

/**
 * 异常基类
 */
public class CommonException extends RuntimeException {

    private Integer code;

    public CommonException(Integer code) {
        super();
        this.code = code;
    }

    public CommonException(String message) {
        super(message);
        this.code = ResultEnum.ERROR.getCode();
    }

    public CommonException(Integer code, String message) {
        super(message);
        this.code = code;
    }

    public CommonException(Integer code, Throwable cause, String message) {
        super(message, cause);
        this.code = code;
    }

    public CommonException(Integer code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }
}