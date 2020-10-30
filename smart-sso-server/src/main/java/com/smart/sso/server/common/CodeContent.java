package com.smart.sso.server.common;

import java.io.Serializable;

public class CodeContent implements Serializable {

	private static final long serialVersionUID = -1332598459045608781L;

	private String service;
	private String tgt;
	
	public CodeContent(String service, String tgt) {
		this.service = service;
		this.tgt = tgt;
	}

	public String getService() {
		return service;
	}

	public void setService(String service) {
		this.service = service;
	}

	public String getTgt() {
		return tgt;
	}

	public void setTgt(String tgt) {
		this.tgt = tgt;
	}
}