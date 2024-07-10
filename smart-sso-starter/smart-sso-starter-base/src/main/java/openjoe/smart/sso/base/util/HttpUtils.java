package openjoe.smart.sso.base.util;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Http请求工具
 *
 * @author Joe
 */
public class HttpUtils {

    private static final Logger logger = LoggerFactory.getLogger(HttpUtils.class);

    public static String get(String url, Map<String, String> paramMap) {
        String result = null;
        CloseableHttpResponse response = null;
        String realUrl = url;
        CloseableHttpClient httpClient = HttpClients.createDefault();
        try {
            if (paramMap != null && !paramMap.isEmpty()) {
                List<NameValuePair> params = new ArrayList<>();
                for (Map.Entry<String, String> entry : paramMap.entrySet()) {
                    params.add(new BasicNameValuePair(entry.getKey(), entry.getValue()));
                }
                String paramStr = EntityUtils.toString(new UrlEncodedFormEntity(params, "UTF-8"));
                realUrl += "?" + paramStr;
            }
            HttpGet httpGet = new HttpGet(realUrl);
            response = httpClient.execute(httpGet);
            if (response != null && response.getStatusLine().getStatusCode() == 200) {
                HttpEntity entity = response.getEntity();
                if (entity != null) {
                    result = EntityUtils.toString(entity);
                }
                logger.debug("http get url: {}, paramMap: {}, result: {}", url, JsonUtils.toString(paramMap), result);
            }
            return result;
        } catch (Exception e) {
            logger.error("http get url: {}, paramMap: {}, result: {}", url, JsonUtils.toString(paramMap), result, e);
        } finally {
            try {
                httpClient.close();
                if (response != null) {
                    response.close();
                }
            } catch (IOException e) {
                logger.error("", e);
            }
        }
        return null;
    }

    public static String get(String url) {
        return get(url, null);
    }

    public static String post(String url, Map<String, String> paramMap, Map<String, String> headerMap) {
        HttpPost httpPost = null;
        CloseableHttpClient httpClient = null;
        try {
            httpPost = new HttpPost(url);
            if (paramMap != null && !paramMap.isEmpty()) {
                List<NameValuePair> formParams = new ArrayList<>();
                for (Map.Entry<String, String> entry : paramMap.entrySet()) {
                    formParams.add(new BasicNameValuePair(entry.getKey(), entry.getValue()));
                }
                httpPost.setEntity(new UrlEncodedFormEntity(formParams, "UTF-8"));
            }
            RequestConfig requestConfig = RequestConfig.custom().setSocketTimeout(5000).setConnectTimeout(5000).build();
            httpPost.setConfig(requestConfig);

            if (headerMap != null && !headerMap.isEmpty()) {
                for (Map.Entry<String, String> headerItem : headerMap.entrySet()) {
                    httpPost.setHeader(headerItem.getKey(), headerItem.getValue());
                }
            }

            httpClient = HttpClients.custom().disableAutomaticRetries().build();

            HttpResponse response = httpClient.execute(httpPost);
            HttpEntity entity = response.getEntity();
            if (entity != null && response.getStatusLine().getStatusCode() == 200) {
                String result = EntityUtils.toString(entity, "UTF-8");
                EntityUtils.consume(entity);
                logger.debug("http post url: {}, result: {}", url, result);
                return result;
            }
            return null;
        } catch (Exception e) {
            logger.error("http post url: {}, paramMap: {}", url, paramMap, e);
            return null;
        } finally {
            if (httpPost != null) {
                httpPost.releaseConnection();
            }
            if (httpClient != null) {
                try {
                    httpClient.close();
                } catch (IOException e) {
                    logger.error("", e);
                }
            }
        }
    }

    public static String post(String url, Map<String, String> paramMap) {
        return post(url, paramMap, null);
    }

    public static String postHeader(String url, Map<String, String> headerMap) {
        return post(url, null, headerMap);
    }
}