package openjoe.smart.sso.server.service.impl;

import openjoe.smart.sso.server.dto.LoginUserDTO;
import openjoe.smart.sso.server.entity.App;
import openjoe.smart.sso.server.entity.TicketGrantingTicketContent;
import openjoe.smart.sso.server.entity.User;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import openjoe.smart.sso.server.service.AppService;
import openjoe.smart.sso.server.service.LoginUserService;
import openjoe.smart.sso.server.service.UserService;
import openjoe.smart.sso.server.stage.core.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

@Service("loginUserService")
public class LoginUserServiceImpl implements LoginUserService {

    @Autowired
    private UserService userService;
    @Autowired
    private AppService appService;
    @Autowired
    private AbstractTicketGrantingTicketManager tgtManager;
    @Autowired
    private AbstractTokenManager tokenManager;

    @Override
    public Page<LoginUserDTO> selectPage(String account, String name, Long current, Long size) {
        Set<Long> userIds = null;
        if (StringUtils.hasLength(account) || StringUtils.hasLength(name)) {
            userIds = userService.selectUserIds(account, name);
        }
        Map<String, TicketGrantingTicketContent> tgtMap = tgtManager.getTgtMap(userIds, current, size);
        List<LoginUserDTO> list = convertList(tgtMap);
        return Page.of(current, size, list);
    }

    private List<LoginUserDTO> convertList(Map<String, TicketGrantingTicketContent> tgtMap) {
        if (CollectionUtils.isEmpty(tgtMap)) {
            return Collections.emptyList();
        }
        Map<Long, User> userMap = userService.selectMapByIds(
                tgtMap.values().stream().map(TicketGrantingTicketContent::getUserId).collect(Collectors.toSet()));
        Map<String, Set<String>> clientIdMap = tokenManager.getClientIdMapByTgt(tgtMap.keySet());
        Map<String, App> appMap = appService.selectMapByClientIds(clientIdMap.values().stream()
                .flatMap(Set::stream)
                .collect(Collectors.toSet()));

        List<LoginUserDTO> dtoList = new ArrayList<>();
        tgtMap.forEach((tgt, tgtContent) -> {
            LoginUserDTO dto = new LoginUserDTO();
            dto.setTgt(tgt);
            User user = userMap.get(tgtContent.getUserId());
            dto.setId(user.getId());
            dto.setName(user.getName());
            dto.setAccount(user.getAccount());
            dto.setCreateTime(new Date(tgtContent.getCreateTime()));
            Set<String> clientIds = clientIdMap.get(tgt);
            dto.setApps(clientIds.stream().map(
                    clientId -> appMap.get(clientId).getCode()).collect(Collectors.joining(",")));
            dtoList.add(dto);
        });
        return dtoList;
    }

    @Override
    public void logout(List<String> tgtList) {
        tgtList.forEach(tgt -> tgtManager.invalidate(tgt));
    }
}
