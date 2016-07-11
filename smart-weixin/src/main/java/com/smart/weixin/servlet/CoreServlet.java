package com.smart.weixin.servlet;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.weixin.listener.CoreListener;
import com.smart.weixin.util.SignUtils;

/**
 * 核心请求处理类
 */
public class CoreServlet extends HttpServlet {

	private static final long serialVersionUID = 4440739483644821986L;

	private static final Logger LOGGER = LoggerFactory.getLogger(CoreServlet.class);

	/**
	 * 确认请求来自微信服务器
	 */
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// 微信加密签名
		String signature = request.getParameter("signature");
		// 时间戳
		String timestamp = request.getParameter("timestamp");
		// 随机数
		String nonce = request.getParameter("nonce");
		// 随机字符串
		String echostr = request.getParameter("echostr");

		PrintWriter out = response.getWriter();
		// 通过检验signature对请求进行校验，若校验成功则原样返回echostr，表示接入成功，否则接入失败
		if (SignUtils.checkSignature(signature, timestamp, nonce)) {
			out.print(echostr);
			out.close();
			LOGGER.warn("--------------- doGet 公众平台接入成功  ---------------");
		}
		else {
			LOGGER.error("--------------- doGet 公众平台接入失败  ---------------");
		}
		out = null;
	}

	/**
	 * 处理微信服务器发来的消息
	 */
	public void doPost(HttpServletRequest request, HttpServletResponse response) {
		try {
			// 将请求、响应的编码均设置为UTF-8（防止中文乱码）
			request.setCharacterEncoding("UTF-8");
			response.setCharacterEncoding("UTF-8");

			// 调用核心业务类接收消息、处理消息
			String respMessage = CoreListener.process(request);

			// 响应消息
			PrintWriter out = response.getWriter();
			out.print(respMessage);
			out.close();
		}
		catch (Exception e) {
			LOGGER.error("处理微信服务器异常", e);
		}
	}
}
