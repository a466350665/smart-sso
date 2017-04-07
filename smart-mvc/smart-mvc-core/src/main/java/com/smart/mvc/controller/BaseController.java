package com.smart.mvc.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.smart.mvc.util.StringUtils;

/**
 * Controller基类
 * 
 * @author Joe
 */
public class BaseController {

	private Integer[] getAjaxIds(final String str, final String separator) {
		Integer[] ids = null;
		if (str != null) {
			String[] strs = str.split(separator);
			ids = new Integer[strs.length];
			for (int i = 0, length = strs.length; i < length; i++) {
				ids[i] = Integer.valueOf(strs[i]);
			}
		}
		return ids;
	}

	protected List<Integer> getAjaxIds(final String id) {
		return StringUtils.isBlank(id) ? new ArrayList<Integer>(0) : Arrays.asList(getAjaxIds(id, ","));
	}
}