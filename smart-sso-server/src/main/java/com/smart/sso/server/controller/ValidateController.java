package com.smart.sso.server.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.sso.client.dto.RpcUserDto;
import com.smart.sso.client.model.Result;
import com.smart.sso.server.common.ServiceTicketManager;
import com.smart.sso.server.common.TicketGrantingTicketManager;

/**
 * ticket验证管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/validate")
public class ValidateController {

    @Autowired
    private ServiceTicketManager serviceTicketManager;
    @Autowired
    private TicketGrantingTicketManager ticketGrantingTicketManager;

    /**
     * 验证
     * 
     * @param service
     * @param request
     * @return
     */
    @SuppressWarnings("rawtypes")
    @ResponseBody
    @RequestMapping(method = RequestMethod.GET)
    public Result login(@RequestParam String ticket) {
        String tgt = serviceTicketManager.validate(ticket);
        if (StringUtils.isEmpty(tgt)) {
            return null;
        }
        RpcUserDto user = ticketGrantingTicketManager.validate(tgt);
        if (user == null) {
            return Result.createError("ticket有误或已过期");
        } else {
            return Result.createSuccess(user);
        }
    }
}