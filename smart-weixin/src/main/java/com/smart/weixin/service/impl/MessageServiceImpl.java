package com.smart.weixin.service.impl;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;

import com.smart.mvc.dao.mybatis.Dao;
import com.smart.mvc.model.PersistentObject;
import com.smart.mvc.service.mybatis.Service;
import com.smart.mvc.service.mybatis.impl.ServiceImpl;
import com.smart.weixin.message.response.BaseMessage;

/**
 * 消息Service基类，实现了数据的CRUD
 * 
 * @param <DAO>
 * @param <T>
 * @param <ID>
 */
@SuppressWarnings("rawtypes")
public class MessageServiceImpl<DAO extends Dao, T extends PersistentObject, ID extends Serializable>
		extends ServiceImpl<DAO, T, ID> implements Service<DAO, T, ID> {

	/**
	 * 设置回复消息公有参数
	 * 
	 * @param map
	 * @param message
	 */
	protected void setMessageDetail(Map<String, String> map, BaseMessage message) {
		message.setToUserName(map.get("FromUserName"));
		message.setFromUserName(map.get("ToUserName"));
		message.setCreateTime(new Date().getTime());
		message.setFuncFlag(0);
	}

	@Override
	public void setDao(DAO dao) {
	}
}
