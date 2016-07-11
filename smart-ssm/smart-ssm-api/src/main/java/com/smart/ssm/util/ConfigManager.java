package com.smart.ssm.util;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.ssm.interceptor.StopWatchHandlerInterceptor;

/**
 * XML配置文件管理器
 * 
 * @author Joe
 */
@SuppressWarnings("unchecked")
public class ConfigManager {

	private static final Logger LOGGER = LoggerFactory.getLogger(StopWatchHandlerInterceptor.class);

	private static Map<String, Document> documentMap = new ConcurrentHashMap<String, Document>();

	private static Document createDocument(String fileName) {
		Document document = documentMap.get(fileName);
		if (document == null) {
			try {
				String configFilePath = Thread.currentThread().getContextClassLoader().getResource("").toURI()
						.getPath()
						+ fileName;
				SAXReader saxReader = new SAXReader();
				document = saxReader.read(new File(configFilePath));
				documentMap.put(fileName, document);
			}
			catch (Exception e) {
				LOGGER.error("xml file {} analyzing error!", fileName, e);
			}
		}
		return document;
	}

	public static Map<String, String> getDocumentMap(String namespace, String fileName) {
		Document document = createDocument(fileName);

		Map<String, String> map = new HashMap<String, String>();
		List<Element> list = document.selectNodes("/config/" + namespace + "/item");
		if (list != null) {
			for (Element e : list) {
				map.put(e.attributeValue("key"), e.attributeValue("value"));
			}
		}
		return map;
	}

	public static String getValue(String key, String namespace, String fileName, String cacheName, CacheConfig cache) {
		Map<String, String> map = null;
		// 没配置缓存
		if (cache == null) {
			LOGGER.warn("has not been cache config");
			map = getDocumentMap(namespace, fileName);
			return map.get(key);
		}
		// 有配置缓存
		map = (Map<String, String>) cache.getCache(cacheName, namespace);
		if (map == null) {
			map = getDocumentMap(namespace, fileName);
			cache.putCache(cacheName, namespace, map);
		}
		return map.get(key);
	}
}