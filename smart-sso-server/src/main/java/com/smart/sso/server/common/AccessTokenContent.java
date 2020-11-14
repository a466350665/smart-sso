package com.smart.sso.server.common;

import com.smart.sso.server.enums.ClientTypeEnum;

public class AccessTokenContent extends CodeContent {

	private static final long serialVersionUID = -1332598459045608781L;

	public AccessTokenContent(String tgt, ClientTypeEnum clientType, String redirectUri) {
		super(tgt, clientType, redirectUri);
	}
}