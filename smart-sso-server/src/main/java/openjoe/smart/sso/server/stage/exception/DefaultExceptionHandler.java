package openjoe.smart.sso.server.stage.exception;

import openjoe.smart.sso.server.stage.core.Result;
import openjoe.smart.sso.server.stage.core.ResultEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.core.annotation.Order;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

/**
 * 公共异常拦截处理
 */
@Order(100)
@ConditionalOnMissingBean(name = {"globalExceptionHandler"})
@RestControllerAdvice
public class DefaultExceptionHandler {

    private static final Logger log = LoggerFactory.getLogger(DefaultExceptionHandler.class);

    /**
     * 自定义异常处理
     *
     * @param e
     * @return
     */
    @ExceptionHandler(CommonException.class)
    @ResponseBody
    public Object handleException(CommonException e) {
        return new Result<>(e.getCode(), e.getMessage());
    }

    /**
     * 未知异常
     *
     * @param e
     * @return
     */
    @ExceptionHandler(Exception.class)
    @ResponseBody
    public Object handleException(Exception e) {
        log.error("global exception.", e);
        return Result.error();
    }

    /**
     * 请求参数缺失异常
     *
     * @param e
     * @return
     */
    @ExceptionHandler(MissingServletRequestParameterException.class)
    @ResponseBody
    public Object handleException(MissingServletRequestParameterException e) {
        log.error("parameter exception.", e);
        return new Result(ResultEnum.VALIDATION_ERROR.getCode(), e.getMessage());
    }

    /**
     * 请求参数类型异常
     *
     * @param e
     * @return
     */
    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    @ResponseBody
    public Object handleException(MethodArgumentTypeMismatchException e) {
        log.error("parameter exception.", e);
        return new Result(ResultEnum.VALIDATION_ERROR.getCode(), e.getMessage());
    }
}