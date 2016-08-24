package com.smart.mvc.util.hibernate;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.List;

/**
 * <b>Title:</b>ProPertyUtils.java<br>
 * <b>Description:</b><br>
 * <b>Copyright:</b>Copyright (c) 2014 <br>
 * <b>Company:</b>佛山广速网络科技有限公司<br>
 * @author 葛传艺
 * @version 0.1 2014-4-9 下午04:44:14
 */
public class ProPertyUtils {
	public static String valProperty(Class<?> clazz, String... property) {
		PropertyDescriptor[] pr = getPropertyDescriptors(clazz);
		String[] newStr = null;
		if (property.length > 1) {
			newStr = new String[property.length - 1];
			for (int i = 1; i < property.length; i++) {
				newStr[i - 1] = property[i];
			}
		}
		for (PropertyDescriptor p : pr) {
			if (property[0].equals(p.getName()) && 0 == (property.length - 1)) {
				return p.getName();
			}
			else if (property[0].equals(p.getName()) && 0 < property.length) {
				return valProperty(p.getPropertyType(), newStr) != null ? p
						.getName()
						+ "." + valProperty(p.getPropertyType(), newStr) : null;
			}
		}
		return null;
	}

	public static  String valProperty(Class<?> T, String property) {
		return valProperty(T, property.split("\\."));
	}

	public static PropertyDescriptor[] getPropertyDescriptors(Class<?> clazz) {
		BeanInfo beanInfo = null;
		try {
			beanInfo = Introspector.getBeanInfo(clazz);
		}
		catch (IntrospectionException e) {
			return null;
		}
		return beanInfo.getPropertyDescriptors();
	}
	
	public static List<String> getPropertyList(Class<?> T) {
		PropertyDescriptor[] pr = getPropertyDescriptors(T);
		List<String> propertys = new ArrayList<String>();
		for (PropertyDescriptor p : pr) {
			if(!p.getName().equals("class")) {
				propertys.add(p.getName());
			}
		}
		return propertys;
	}
}
