package openjoe.smart.sso.server.stage.core;

import java.beans.Transient;

/**
 * 响应结果
 */
public class Result<T> {

	/**
	 * 响应码
	 */
	private Integer code;

	/**
	 * 消息
	 */
	private String message;

	/**
	 * 数据
	 */
	private T data;

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
		return new Result<>(ResultEnum.SUCCESS.getCode(), ResultEnum.SUCCESS.getMessage());
	}

	public static <T> Result<T> success(T data) {
		Result<T> r = success();
		r.setData(data);
		return r;
	}

	public static <T> Result<T> error() {
		return new Result<>(ResultEnum.ERROR.getCode(), ResultEnum.ERROR.getMessage());
	}

	public Integer getCode() {
		return code;
	}

	public Result<T> setCode(Integer code) {
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

	public T getData() {
		return data;
	}

	public Result<T> setData(T data) {
		this.data = data;
		return this;
	}

	@Transient
	public boolean isSuccess() {
		return ResultEnum.SUCCESS.getCode().equals(this.code);
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