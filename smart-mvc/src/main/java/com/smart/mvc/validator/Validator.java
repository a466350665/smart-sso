package com.smart.mvc.validator;

import com.smart.mvc.exception.ValidateException;
import com.smart.mvc.util.StringUtils;
import com.smart.mvc.util.ValidateUtils;

/**
 * 验证器
 * 
 * @author Joe
 */
public enum Validator {

	/**
	 * 非空验证
	 */
	NOT_BLANK {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (StringUtils.isBlank(value)) {
				throw new ValidateException(name + "不能为空！");
			}
		}
	},
	/**
	 * 中文验证
	 */
	CHINESE {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isChinese(value)) {
				throw new ValidateException(name + "必须为中文！");
			}
		}
	},
	/**
	 * 整数验证
	 */
	INT {
		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isInteger(value)) {
				throw new ValidateException(name + "必须为整数！");
			}
		}
	},
	/**
	 * 日期验证
	 */
	DATE {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isDate(value)) {
				throw new ValidateException(name + "格式不对！");
			}
		}
	},
	/**
	 * 身份号验证
	 */
	IDNO {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isIdNo(value)) {
				throw new ValidateException(name + "不符合身份证号格式，请检查！");
			}
		}
	},
	IP {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isIp(value)) {
				throw new ValidateException(name + "不符合IP地址格式，请检查！");
			}
		}
	},
	/**
	 * 邮件验证
	 */
	EMAIL {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isEmail(value)) {
				throw new ValidateException(name + "格式不正确，请检查！");
			}
		}
	},
	/**
	 * 手机验证
	 */
	MOBILE {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isMobile(value)) {
				throw new ValidateException(name + "格式不正确，请检查！");
			}

		}
	},
	/**
	 * 密码验证
	 */
	PASSWORD {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isPassword(value)) {
				throw new ValidateException(name + "格式不正确，请检查！");
			}
		}
	},
	/**
	 * 姓名验证
	 */
	PERSONNAME {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isPersonName(value)) {
				throw new ValidateException(name + "格式不正确，请检查！");
			}

		}
	},
	/**
	 * 用户名验证
	 */
	USERNAME {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isUsername(value)) {
				throw new ValidateException(name + "格式不正确，请检查！");
			}

		}
	},

	/**
	 * 金额格式
	 */
	MONEY {

		@Override
		public void validate(String name, String value) throws ValidateException {
			if (!ValidateUtils.isMoney(value)) {
				throw new ValidateException(name + "格式不正确，请检查！");
			}
		}
	};
	/**
	 * 参数校验
	 * 
	 * @param name
	 *            参数的中文名称
	 * @param value
	 *            参数的值
	 * @throws Exception
	 */
	public abstract void validate(String name, String value) throws ValidateException;

	/**
	 * 根据验证器的名称获取验证器
	 * 
	 * @param validatorName
	 * @return
	 */
	public static Validator getValidator(Validator v) {
		for (Validator validator : values()) {
			if (validator == v) {
				return validator;
			}
		}
		return null;
	}
}
