package com.smart.mvc.util.hibernate;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * 获取对象泛型参数Class
 */
public class DaoUtils {

	public static Class<?> getSuperClassType(Class<?> clazz) {
		return getSuperClassType(clazz, 0);
	}

	public static Class<?> getSuperClassType(Class<?> clazz, int index)
			throws IndexOutOfBoundsException {
		Type genType = clazz.getGenericSuperclass();
		if (!(genType instanceof ParameterizedType)) {
			throw new IllegalArgumentException(clazz.getSimpleName()
					+ "'s superclass not ParameterizedType");
		}
		Type[] params = ((ParameterizedType) genType).getActualTypeArguments();
		if (index >= params.length || index < 0) {
			throw new IllegalArgumentException("Index: " + index + ", Size of "
					+ clazz.getSimpleName() + "'s Parameterized Type: "
					+ params.length);
		}
		if (!(params[index] instanceof Class<?>)) {
			throw new IllegalArgumentException(
					clazz.getSimpleName()
							+ " not set the actual class on superclass generic parameter");
		}
		return (Class<?>) params[index];
	}
	
	/**
	 * 获取当前Class参数
	 */
	public static Class<?> getClassType(Class<?> clazz, int index) {
		Type[] types = clazz.getGenericInterfaces();
		for (Type type : types) {
			if (type instanceof ParameterizedType) {
				ParameterizedType ptype = (ParameterizedType) type;
				return (Class<?>) ptype.getActualTypeArguments()[index];
			}
		}
		return Object.class;
	}
}
