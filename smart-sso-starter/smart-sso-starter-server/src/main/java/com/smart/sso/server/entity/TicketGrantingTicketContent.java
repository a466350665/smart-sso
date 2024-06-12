package com.smart.sso.server.entity;

import com.smart.sso.base.entity.Userinfo;

/**
 * 授权存储信息
 * 
 * @author Joe
 */
public class TicketGrantingTicketContent {

	private Userinfo userinfo;

	public TicketGrantingTicketContent() {
	}

	public TicketGrantingTicketContent(Userinfo userinfo) {
		this.userinfo = userinfo;
	}

	public Userinfo getUserinfo() {
		return userinfo;
	}

	public void setUserinfo(Userinfo userinfo) {
		this.userinfo = userinfo;
	}
}