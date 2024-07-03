package openjoe.smart.sso.base.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Json处理工具
 *
 * @author Joe
 */
public class JsonUtils {

    private static final Logger logger = LoggerFactory.getLogger(JsonUtils.class);
    private static ObjectMapper mapper = new ObjectMapper();

    public static String toString(Object object) {
        try {
            return mapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            logger.error("serializeToString has error, object:{}", object, e);
        }
        return "";
    }

    public static <T> T parseObject(String jsonString, Class<T> cls) {
        try {
            return mapper.readValue(jsonString, cls);
        } catch (Exception e) {
            logger.error("deserialize has error, jsonString:{}, class:{}", jsonString, cls, e);
        }
        return null;
    }

    public static <T> T parseObject(String jsonString, TypeReference<T> tr) {
        try {
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            return mapper.readValue(jsonString, tr);
        } catch (JsonProcessingException e) {
            logger.error("deserialize has error, jsonString:{}, tr:{}", jsonString, tr, e);
        }
        return null;
    }

    public static ObjectMapper getObjectMapper() {
        return mapper;
    }
}