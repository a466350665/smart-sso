package com.smart.sso.base.entity;

/**
 * 对象携带过期时间功能的包装器
 * @param <T>
 *
 * @author Joe
 */
public class ObjectWrapper<T> {
	private T object;

	/**
	 * 过期时间
	 */
	private Long expired;

	public ObjectWrapper() {
		super();
	}

	public ObjectWrapper(T object, Long expired) {
		super();
		this.object = object;
		this.expired = expired;
	}

	public T getObject() {
		return object;
	}

	public void setObject(T object) {
		this.object = object;
	}

	public Long getExpired() {
		return expired;
	}

	public void setExpired(Long expired) {
		this.expired = expired;
	}
}