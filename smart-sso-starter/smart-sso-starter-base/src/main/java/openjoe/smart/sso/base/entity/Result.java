package openjoe.smart.sso.base.entity;

import java.beans.Transient;

/**
 * 返回结果
 *
 * @author Joe
 */
public class Result<T> {

    /**
     * 成功
     */
    public static final String SUCCESS_CODE = "000000";

    /**
     * 系统错误
     */
    public static final String ERROR_CODE = "000001";

    /**
     * 结果体
     */
    private T data;

    /**
     * 状态码
     */
    private String code;

    /**
     * 信息
     */
    private String message;

    public Result() {
    }

    public Result(String code, String message) {
        this.code = code;
        this.message = message;
    }

    public Result(String code, String message, T data) {
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

    public String getCode() {
        return code;
    }

    public Result<T> setCode(String code) {
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
