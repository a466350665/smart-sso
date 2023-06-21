package com.smart.sso.server;

import com.smart.mvc.constant.ResultConstant;
import com.smart.mvc.model.Result;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import javax.servlet.http.HttpServletRequest;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import java.text.MessageFormat;
import java.util.Set;

@RestControllerAdvice(basePackages = {"com.smart.sso.server.controller"})
public class GlobalExceptionHandler {

    @ExceptionHandler(value = {Throwable.class})
    Result handleException(Throwable e, HttpServletRequest request) {
        Result result = Result.create(ResultConstant.ERROR);
        if (e instanceof MissingServletRequestParameterException) {
            result.setMessage(
                MessageFormat.format("缺少参数{0}", ((MissingServletRequestParameterException)e).getParameterName()));
        }
        else if (e instanceof ConstraintViolationException) {
            // 单个参数校验异常
            Set<ConstraintViolation<?>> sets = ((ConstraintViolationException)e).getConstraintViolations();
            if (!CollectionUtils.isEmpty(sets)) {
                StringBuilder sb = new StringBuilder();
                sets.forEach(error -> {
                    if (error instanceof FieldError) {
                        sb.append(((FieldError)error).getField()).append(":");
                    }
                    sb.append(error.getMessage()).append(";");
                });
                String msg = sb.toString();
                //                msg = StringUtils.substring(msg, 0, msg.length() -1);
                result.setMessage(msg);
            }
        }
        return result;
    }
}
