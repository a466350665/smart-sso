package com.smart.sso.server.validator;

import java.lang.annotation.*;

/**
 * 自定义请求参数注解
 * 
 * @author Joe
 */
@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface ValidateParam {
    
    /**
     * 参数的描述名称
     * @return
     */
    String name() default "";

	/**
	 * 验证器
	 * @return
	 */
	Validator[] value() default {};
	
	/**
     * 默认值
     * @return
     */
    String defaultValue() default "";
}