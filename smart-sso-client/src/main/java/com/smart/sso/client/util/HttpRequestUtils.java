package com.smart.sso.client.util;

import java.io.IOException;

import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Http请求
 * 
 * @author Joe
 */
public class HttpRequestUtils {

	private static final Logger logger = LoggerFactory.getLogger(HttpRequestUtils.class);

	public static String get(String url) {
		String result = null;
		CloseableHttpClient httpClient = HttpClients.createDefault();
		HttpGet get = new HttpGet(url);
		CloseableHttpResponse response = null;
		try {
			response = httpClient.execute(get);
			if (response != null && response.getStatusLine().getStatusCode() == 200) {
				HttpEntity entity = response.getEntity();
				if (entity != null) {
					result = EntityUtils.toString(entity);
				}
				logger.info("url: {}, result: {}", url, result);
			}
			return result;
		}
		catch (Exception e) {
			logger.error("url: {}, result: {}", url, result, e);
		}
		finally {
			try {
				httpClient.close();
				if (response != null) {
					response.close();
				}
			}
			catch (IOException e) {
				logger.error("", e);
			}
		}
		return null;
	}
}