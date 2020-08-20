package com.smart.mvc.model;

import java.util.Collection;

import com.smart.mvc.enums.QueryTypeEnum;

/**
 * SQL查询条件
 * 
 * like the following:
 * Condition c = Condition.create()
 *      .sql("substr(name, 1, 2) = 'Jo'")
 *      .isNotNull("id")
 *      .in("id", Lists.newArrayList(1, 2))
 *      .between("id", 1, 10)
 *      .like("name", "Joe")
 *      .eq("account", "admin")
 *      .like("office_name", "研发")
 *      .orderBy("name desc, account asc");
 * 
 * @author Joe
 */
public interface Conditionable {
	
	Condition condition();
	
	default Condition isNull(String column) {
		return condition().addCriteria(column, QueryTypeEnum.IS_NULL);
	}

	default Condition isNull(boolean checker, String column) {
		return checker ? isNull(column) : condition();
	}
	
	default Condition isNotNull(String column) {
		return condition().addCriteria(column, QueryTypeEnum.IS_NOT_NULL);
	}

	default Condition isNotNull(boolean checker, String column) {
		return checker ? isNotNull(column) : condition();
	}
	
	default Condition eq(String column, Object value) {
		return condition().addCriteria(column, QueryTypeEnum.EQUAL, value);
	}
	
	default Condition eq(boolean checker, String column, Object value) {
		return checker ? eq(column, value) : condition();
	}
	
	default Condition ne(String column, Object value) {
		return condition().addCriteria(column, QueryTypeEnum.NOT_EQUAL, value);
	}
	
	default Condition ne(boolean checker, String column, Object value) {
		return checker ? ne(column, value) : condition();
	}
	
	default Condition gt(String column, Object value) {
		return condition().addCriteria(column, QueryTypeEnum.GREATER, value);
	}
	
	default Condition gt(boolean checker, String column, Object value) {
		return checker ? gt(column, value) : condition();
	}
	
	default Condition ge(String column, Object value) {
		return condition().addCriteria(column, QueryTypeEnum.GREATER_EQUAL, value);
	}
	
	default Condition ge(boolean checker, String column, Object value) {
		return checker ? ge(column, value) : condition();
	}
	
	default Condition lt(String column, Object value) {
		return condition().addCriteria(column, QueryTypeEnum.LESS, value);
	}
	
	default Condition lt(boolean checker, String column, Object value) {
		return checker ? lt(column, value) : condition();
	}
	
	default Condition le(String column, Object value) {
		return condition().addCriteria(column, QueryTypeEnum.LESS_EQUAL, value);
	}
	
	default Condition le(boolean checker, String column, Object value) {
		return checker ? le(column, value) : condition();
	}
	
	default Condition in(String column, Collection<?> value) {
		return condition().addCriteria(column, QueryTypeEnum.IN, value);
	}
	
	default Condition in(boolean checker, String column, Collection<?> value) {
		return checker ? in(column, value) : condition();
	}
	
	default Condition notIn(String column, Collection<?> value) {
		return condition().addCriteria(column, QueryTypeEnum.NOT_IN, value);
	}
	
	default Condition notIn(boolean checker, String column, Collection<?> value) {
		return checker ? notIn(column, value) : condition();
	}
	
	default Condition between(String column, Object v1, Object v2) {
		return condition().addCriteria(column, QueryTypeEnum.BETWEEN, v1, v2);
	}
	
	default Condition between(boolean checker, String column, Object v1, Object v2) {
		return checker ? between(column, v1, v2) : condition();
	}
	
	default Condition notBetween(String column, Object v1, Object v2) {
		return condition().addCriteria(column, QueryTypeEnum.NOT_BETWEEN, v1, v2);
	}
	
	default Condition notBetween(boolean checker, String column, Object v1, Object v2) {
		return checker ? notBetween(column, v1, v2) : condition();
	}
	
	default Condition like(String column, String value) {
		return condition().addCriteria(column, QueryTypeEnum.LIKE, "%" + value + "%");
	}
	
	default Condition like(boolean checker, String column, String value) {
		return checker ? like(column, value) : condition();
	}
	
	default Condition notLike(String column, String value) {
		return condition().addCriteria(column, QueryTypeEnum.NOT_LIKE, "%" + value + "%");
	}
	
	default Condition notLike(boolean checker, String column, String value) {
		return checker ? notLike(column, value) : condition();
	}
	
	default Condition sql(String sql) {
		return condition().addCriteria(sql);
	}
	
	default Condition orderBy(String orderBy) {
		return condition().addOrderBy(orderBy);
	}
	
	default Condition orderBy(boolean checker, String orderBy) {
		return checker ? orderBy(orderBy) : condition();
	}
}