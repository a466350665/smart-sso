package com.smart.sso.server.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Controller基类
 * 
 * @author Joe
 */
public class BaseController {
	/**
	 * 日志对象
	 */
	protected Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * 为Controller方法参数提供String类型分页默认值
     */
    public static final String DEFAULT_PAGE_NO = "1";
    public static final String DEFAULT_PAGE_SIZE = "20";
    
    /**
     * 将逗号分隔的id字符串转为List
     *
     * @param ids
     * @return
     */
	protected List<Integer> convertToIdList(final String ids) {
		return StringUtils.isEmpty(ids) ? Collections.emptyList()
				: Stream.of(ids.split(",")).filter(s -> !StringUtils.isEmpty(s)).map(s -> Integer.valueOf(s.trim()))
						.collect(Collectors.toList());
	}
}