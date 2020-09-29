package com.smart.sso.server.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.smart.sso.client.dto.RpcPermissionDto;
import com.smart.sso.client.dto.RpcUserDto;
import com.smart.sso.client.rpc.AuthenticationRpcService;
import com.smart.sso.server.common.ServiceTicketManager;
import com.smart.sso.server.common.TicketGrantingTicketManager;
import com.smart.sso.server.service.PermissionService;

@Service("authenticationRpcService")
public class AuthenticationRpcServiceImpl implements AuthenticationRpcService {

	@Autowired
	private PermissionService permissionService;
	@Autowired
    private ServiceTicketManager serviceTicketManager;
	@Autowired
    private TicketGrantingTicketManager ticketGrantingTicketManager;

	@Override
	public RpcUserDto validate(String ticket) {
	    String tgt = serviceTicketManager.validate(ticket);
	    if(StringUtils.isEmpty(tgt)) {
	        return null;
	    }
		return ticketGrantingTicketManager.validate(tgt);
	}
	
	@Override
	public List<RpcPermissionDto> selectUserPermissionList(Integer userId, String appCode) {
		return permissionService.selectListByUserId(appCode, userId);
	}
	
	@Override
    public List<RpcPermissionDto> selectApplicationPermissionList(String appCode) {
        return permissionService.selectListByUserId(appCode, null);
    }
}
