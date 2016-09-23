package com.smart.sso.server.controller;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.mvc.model.Result;
import com.smart.mvc.provider.IdProvider;
import com.smart.mvc.provider.PasswordProvider;
import com.smart.mvc.util.CookieUtils;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.client.ApplicationUtils;
import com.smart.sso.server.common.LoginUser;
import com.smart.sso.server.common.Loginable;
import com.smart.sso.server.common.TokenManager;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.service.impl.PermissionSubject;
import com.smart.util.StringUtils;

/**
 * 登录
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/login")
public class LoginController {

	@Resource
	private TokenManager tokenManager;
	@Resource
	private UserService userService;
	@Resource
	private PermissionSubject permissionSubject;

	@RequestMapping(method = RequestMethod.GET)
	public String login(@ValidateParam(name = "返回链接", validators = { Validator.NOT_BLANK }) String backUrl,
			@ValidateParam(name = "应用编码", validators = { Validator.NOT_BLANK }) String appCode,
			HttpServletRequest request) {
		String token = CookieUtils.getCookie(request, "token");
		if (token == null) {
			request.setAttribute("backUrl", backUrl);
			request.setAttribute("appCode", appCode);
			return Loginable.LOGIN_PATH;
		}
		else {
			LoginUser loginUser = tokenManager.validate(token);
			if (loginUser != null) {
				// 为应用添加权限主题观察者，以便应用权限修改通知到对应应用更新权限
				permissionSubject.attach(appCode);

				if (StringUtils.isBlank(backUrl)) {
					return Loginable.LOGIN_SUCCESS_PATH;
				}
				else {
					return "redirect:" + authBackUrl(backUrl, token);
				}
			}
			else {
				request.setAttribute("backUrl", backUrl);
				request.setAttribute("appCode", appCode);
				return Loginable.LOGIN_PATH;
			}
		}
	}

	@RequestMapping(method = RequestMethod.POST)
	public String login(@ValidateParam(name = "返回链接", validators = { Validator.NOT_BLANK }) String backUrl,
			@ValidateParam(name = "应用编码", validators = { Validator.NOT_BLANK }) String appCode,
			@ValidateParam(name = "登录名", validators = { Validator.NOT_BLANK }) String account,
			@ValidateParam(name = "密码", validators = { Validator.NOT_BLANK }) String password,
			HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {
		Result result = userService.login(ApplicationUtils.getIpAddr(request), appCode, account,
				PasswordProvider.encrypt(password));
		if (!result.isSuccess()) {
			request.setAttribute(Loginable.VALIDATE_MESSAGE_NAME, result.getMessage());
			request.setAttribute("backUrl", backUrl);
			request.setAttribute("appCode", appCode);
			return Loginable.LOGIN_PATH;
		}
		else {
			User user = (User) result.getData();
			String token = createToken(response, new LoginUser(user.getId(), user.getAccount(), user));

			// 为应用添加权限主题观察者，以便应用权限修改通知到对应应用更新权限
			permissionSubject.attach(appCode);

			// 4 跳转到原请求
			if (StringUtils.isBlank(backUrl)) {
				return Loginable.LOGIN_SUCCESS_PATH;
			}
			else {
				backUrl = URLDecoder.decode(backUrl, "utf-8");
				try {
					if (backUrl != null) {
						return "redirect:" + authBackUrl(backUrl, token);
					}
				}
				catch (Exception e) {
					e.printStackTrace();
				}
			}
			return backUrl;
		}
	}

	private String authBackUrl(String backUrl, String token) {
		StringBuilder sbf = new StringBuilder(backUrl);
		if (backUrl.indexOf("?") > 0) {
			sbf.append("&");
		}
		else {
			sbf.append("?");
		}
		sbf.append(ApplicationUtils.SSO_TOKEN_NAME).append("=").append(token);
		return sbf.toString();
	}

	private String createToken(HttpServletResponse response, LoginUser loginUser) {
		// 生成token
		String token = IdProvider.createUUIDId();

		// 缓存中添加token对应User
		tokenManager.addToken(token, loginUser);

		// Cookie添加token
		Cookie cookie = new Cookie("token", token);
		cookie.setPath("/");
		cookie.setHttpOnly(true);
		response.addCookie(cookie);
		return token;
	}
}