package openjoe.smart.sso.base.entity;

import java.beans.Transient;
import java.io.Serializable;

/**
 * 返回结果
 *
 * @author Joe
 */
public class Result<T> implements Serializable {

    private static final long serialVersionUID = 1956544564021082972L;

    /**
     * 成功
     */
    public static final int SUCCESS_CODE = 1;

    /**
     * 系统错误
     */
    public static final int ERROR_CODE = 9999;

    /**
     * 结果体
     */
    private T data;

    /**
     * 状态码
     */
    private int code;

    /**
     * 信息
     */
    private String message;

    public Result() {
    }

    public Result(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    public Result(Integer code, String message, T data) {
        this(code, message);
        this.data = data;
    }

    public static <T> Result<T> success() {
        return new Result<>(SUCCESS_CODE, "成功");
    }

    public static <T> Result<T> success(T data) {
        Result<T> r = success();
        r.setData(data);
        return r;
    }

    public static <T> Result<T> error() {
        return new Result<>(ERROR_CODE, "未知错误");
    }

    public static <T> Result<T> error(String message) {
        return new Result<>(ERROR_CODE, message);
    }

    public T getData() {
        return data;
    }

    public Result<T> setData(T data) {
        this.data = data;
        return this;
    }

    public int getCode() {
        return code;
    }

    public Result<T> setCode(int code) {
        this.code = code;
        return this;
    }

    public String getMessage() {
        return message;
    }

    public Result<T> setMessage(String message) {
        this.message = message;
        return this;
    }

    @Transient
    public boolean isSuccess() {
        return SUCCESS_CODE == code;
    }

    @Override
    public String toString() {
        return "Result{" +
                "code=" + code +
                ", message='" + message + '\'' +
                ", data=" + data +
                '}';
    }
}
