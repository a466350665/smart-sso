package com.smart.mvc.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.springframework.util.StringUtils;

import com.smart.mvc.enums.QueryTypeEnum;
import com.smart.mvc.enums.ValueTypeEnum;

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
public class Condition implements Conditionable, Serializable {

    private static final long serialVersionUID = 1948446733871975127L;

    private String orderBy;

    private List<Criteria> criteriaList;

    private Condition() {
    	super();
    	criteriaList = new ArrayList<>();
    }
    
    /**
     * 构造方法
     * 
     * @return
     */
    public static Condition create() {
        return new Condition();
    }

    /**
     * 追加排序
     * 
     * @param orderBy
     * @return
     */
    protected Condition addOrderBy(String orderBy) {
        this.orderBy = orderBy;
        return this;
    }

    public String getOrderBy() {
        return orderBy;
    }

    public List<Criteria> getCriteriaList() {
        return criteriaList;
    }

    /**
     * 追加查询条件
     * 
     * @param criteria
     * @return
     */
    protected Condition addCriteria(String criteria) {
        if (StringUtils.isEmpty(criteria)) {
            throw new IllegalArgumentException("criteria cannot be null");
        }
        criteriaList.add(new Criteria(criteria, null, ValueTypeEnum.NO));
        return this;
    }

    /**
     * 追加查询条件
     * 
     * @param column
     * @param queryType
     * @param value
     * @return
     */
    protected Condition addCriteria(String column, QueryTypeEnum queryType, Object... value) {
        Object val = parseTypeValue(column, queryType, value);
        criteriaList.add(new Criteria(column + " " + queryType.getLabel(), val, queryType.getValueType()));
        return this;
    }
    
	private Object parseTypeValue(String property, QueryTypeEnum queryType, Object... value) {
		Object val = null;
		if (StringUtils.isEmpty(property)) {
			throw new IllegalArgumentException("property cannot be null");
		}
		if (queryType == null) {
			throw new IllegalArgumentException("queryType cannot be null");
		}
		if (queryType.getValueType() != ValueTypeEnum.NO) {
			if (value == null) {
				throw new IllegalArgumentException("value cannot be null");
			}
			if (queryType.getValueType() == ValueTypeEnum.SINGLE) {
				val = value[0];
			}
			else if (queryType.getValueType() == ValueTypeEnum.COLLECTION) {
				if (!(value[0] instanceof Collection)) {
					throw new IllegalArgumentException("value type must be Collection");
				}
				else if (((Collection<?>) value[0]).isEmpty()) {
					throw new IllegalArgumentException("value can't be empty");
				}
				val = value[0];
			}
			else if (queryType.getValueType() == ValueTypeEnum.TWO) {
				if (value.length == 2) {
					val = value;
				}
				else {
					throw new IllegalArgumentException("value length must be 2");
				}
			}
		}
		return val;
	}

    public class Criteria extends Item {

        private static final long serialVersionUID = 1681191456328793646L;
        
        private ValueTypeEnum valueType;

        public Criteria(String label, Object value, ValueTypeEnum valueType) {
            super(label, value);
            this.valueType = valueType;
        }

        public boolean isNoValue() {
            return valueType == ValueTypeEnum.NO;
        }

        public boolean isSingleValue() {
            return valueType == ValueTypeEnum.SINGLE;
        }

        public boolean isTwoValue() {
            return valueType == ValueTypeEnum.TWO;
        }

        public boolean isCollectionValue() {
            return valueType == ValueTypeEnum.COLLECTION;
        }
    }

	@Override
	public Condition condition() {
		return this;
	}
}