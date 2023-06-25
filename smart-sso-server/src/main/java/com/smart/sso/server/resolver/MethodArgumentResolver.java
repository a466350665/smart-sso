package com.smart.sso.server.resolver;

import com.smart.sso.server.validator.ValidateParam;
import com.smart.sso.server.validator.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.MethodParameter;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 自定义方法参数解析器
 * 
 * @author Joe
 */
public class MethodArgumentResolver implements HandlerMethodArgumentResolver {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	/**
	 * 参数结构缓存
	 */
	private Map<MethodParameter, ParamInfo> paramInfoCache = new ConcurrentHashMap<>(256);

	@Override
	public boolean supportsParameter(MethodParameter parameter) {
		return parameter.hasParameterAnnotation(ValidateParam.class);
	}

	@Override
	public Object resolveArgument(MethodParameter parameter, ModelAndViewContainer mavContainer,
			NativeWebRequest webRequest, WebDataBinderFactory binderFactory) throws Exception {
		ParamInfo paramInfo = getParamInfo(parameter);

		Object value = null;
		/**
		 * 先param里面取
		 */
		String[] paramValues = webRequest.getParameterValues(paramInfo.paramName);
		if (paramValues != null) {
			value = paramValues.length == 1 ? paramValues[0] : paramValues;
		}
		/**
		 * 如果在request里面没有取到就去attribute里面拿
		 */
		if (value == null) {
			value = webRequest.getAttribute(paramInfo.paramName, RequestAttributes.SCOPE_REQUEST);
		}
		/**
         * 赋于默认值
         */
        if (value == null && !StringUtils.isEmpty(paramInfo.defaultValue)) {
            value = paramInfo.defaultValue;
        }

		/**
		 * 验证器的数据校验
		 */
		if (paramInfo.validators != null) {
			validateValue(value, paramInfo.name, paramInfo.validators);
		}

		/**
		 * 数据类型的强制转换
		 */
		if (binderFactory != null) {
			WebDataBinder binder = binderFactory.createBinder(webRequest, null, paramInfo.name);
			value = binder.convertIfNecessary(value, parameter.getParameterType(), parameter);
		}
		return value;
	}

	/**
	 * 对已经值进行数据校验
	 * 
	 * @param value
	 *            参数的值
	 * @param cName
	 *            参数的中文名称
	 * @param validators
	 *            验证器
	 * @throws Exception
	 */
	private void validateValue(Object value, String cName, Validator[] validators) {
		for (int i = 0; i < validators.length; i++) {
			Validator validator = Validator.getValidator(validators[i]);
			if (validator != null) {
				if (value != null && value.toString().trim() != "") {
					validator.validate(cName, value.toString());
				}
				else if (Validator.NOT_BLANK.equals(validator)) {
					validator.validate(cName, null);
				}
			}
			else {
				logger.error("验证器[" + validators[i] + "],在Validator.java文件中没有定义，请检查！");
			}
		}
	}

	/**
	 * 获取参数所对应的参数信息
	 * 
	 * @param parameter
	 * @return
	 */
	private ParamInfo getParamInfo(MethodParameter parameter) {
		ParamInfo paramInfo = this.paramInfoCache.get(parameter);
		if (paramInfo == null) {
			paramInfo = createParamInfo(parameter);
			this.paramInfoCache.put(parameter, paramInfo);
		}
		return paramInfo;
	}

	/**
	 * 创建参数对应信息
	 * 
	 * @param parameter
	 * @return
	 */
	protected ParamInfo createParamInfo(MethodParameter parameter) {
	    ParamInfo info = new ParamInfo(parameter.getParameterName());
		ValidateParam validateParam = parameter.getParameterAnnotation(ValidateParam.class);
		if (validateParam != null) {
			info.setName(validateParam.name());
			info.setValidators(validateParam.value());
			info.setDefaultValue(validateParam.defaultValue());
		}
		return info;
	}

	/**
	 * 参数的相关信息
	 */
	protected static class ParamInfo {

		private String paramName;

		private String name;
		
		private  String defaultValue;

		private Validator[] validators;
		
		public ParamInfo() {
			super();
		}
		
		public ParamInfo(String paramName) {
			super();
			this.paramName = paramName;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
		
		public String getDefaultValue() {
            return defaultValue;
        }

        public void setDefaultValue(String defaultValue) {
            this.defaultValue = defaultValue;
        }

        public Validator[] getValidators() {
			return validators;
		}

		public void setValidators(Validator[] validators) {
			this.validators = validators;
		}
	}
}