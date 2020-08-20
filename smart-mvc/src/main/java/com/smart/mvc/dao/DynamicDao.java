package com.smart.mvc.dao;

import java.io.Serializable;
import java.util.List;

import org.apache.ibatis.annotations.DeleteProvider;
import org.apache.ibatis.annotations.InsertProvider;
import org.apache.ibatis.annotations.Options;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.SelectProvider;
import org.apache.ibatis.annotations.UpdateProvider;

import com.smart.mvc.model.Condition;
import com.smart.mvc.model.Page;
import com.smart.mvc.provider.DynamicSqlProvider;

/**
 * 动态生成Sql实现Dao接口
 * 
 * @author Joe
 *
 * @param <T>
 */
public interface DynamicDao<T> extends Dao<T>{

	@Override
	@SelectProvider(type = DynamicSqlProvider.class, method="selectByCondition")
	List<T> selectByCondition(@Param("condition") Condition condition, Page<T> p);

	@Override
	@SelectProvider(type = DynamicSqlProvider.class, method="selectById")
	T selectById(Serializable id);

	@Override
	@Options(useGeneratedKeys = true, keyProperty = DynamicSqlProvider.ID)
	@InsertProvider(type = DynamicSqlProvider.class, method = "insert")
	int insert(T t);

	@Override
	@UpdateProvider(type = DynamicSqlProvider.class, method="update")
	int update(T t);

	@Override
	@DeleteProvider(type = DynamicSqlProvider.class, method="deleteByCondition")
	int deleteByCondition(@Param("condition") Condition condition);
}
