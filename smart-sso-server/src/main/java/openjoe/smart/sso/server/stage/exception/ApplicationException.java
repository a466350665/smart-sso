package openjoe.smart.sso.server.stage.exception;

import openjoe.smart.sso.server.stage.core.IErrorCode;
import openjoe.smart.sso.server.stage.core.Message;

/**
 * 应用服务异常
 */
public class ApplicationException extends CommonException {

    public ApplicationException(Integer code) {
        super(code, Message.get(code.toString()));
    }

    public ApplicationException(IErrorCode e) {
        super(e.getCode(), e.getMessage());
    }

    public ApplicationException(Integer code, Object... args) {
        super(code, Message.get(code.toString(), args));
    }

    public ApplicationException(IErrorCode e, Object... args) {
        super(e.getCode(), e.getMessage(args));
    }

    public ApplicationException(Integer code, Throwable cause) {
        super(code, cause, Message.get(code.toString()));
    }

    public ApplicationException(IErrorCode e, Throwable cause) {
        super(e.getCode(), cause, e.getMessage());
    }

    public ApplicationException(Integer code, Throwable cause, Object... args) {
        super(code, cause, Message.get(code.toString(), args));
    }

    public ApplicationException(IErrorCode e, Throwable cause, Object... args) {
        super(e.getCode(), cause, e.getMessage(args));
    }
}