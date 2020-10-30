package com.smart.sso.server.service.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.springframework.stereotype.Service;

import com.smart.sso.client.rpc.Result;
import com.smart.sso.server.service.AppService;

@Service("appService")
public class AppServiceImpl implements AppService {

	private static Map<String, String> appMap;

	static {
		appMap = new HashMap<>();
		appMap.put("server1", "123456");
		appMap.put("demo1", "123456");
	}

	@Override
	public boolean exists(String appId) {
		return appMap.containsKey(appId);
	}

	@Override
	public Result<Void> validate(String appId, String appSecret) {
		for (Entry<String, String> app : appMap.entrySet()) {
			if (app.getKey().equals(appId)) {
				if(app.getValue().equals(appSecret)) {
					return Result.success();
				}
				else {
					return Result.createError("appSecret有误");
				}
			}
		}
		return Result.createError("appId不存在");
	}
}
