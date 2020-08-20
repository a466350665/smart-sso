package com.smart.mvc.service.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.smart.mvc.dao.Dao;
import com.smart.mvc.model.Condition;
import com.smart.mvc.model.Page;
import com.smart.mvc.model.PersistentObject;
import com.smart.mvc.provider.DynamicSqlProvider;
import com.smart.mvc.service.BaseService;
import com.smart.mvc.util.ConvertUtils;


/**
 * Service基类，实现了数据的CRUD
 * 
 * @param <DAO>
 * @param <T>
 * @param <ID>
 * @author Joe
 */
public class BaseServiceImpl<DAO extends Dao<T>, T extends PersistentObject, ID extends Serializable>
		implements BaseService<T, ID> {

	protected final Logger logger = LoggerFactory.getLogger(getClass());
	
	/**
	 * 自动根据子类传入DAO类型识别注入
	 */
	@Autowired
	protected DAO dao;

	@Override
    public List<T> selectAll() {
        return selectList(null);
    }
	
	protected List<T> selectList(Condition c) {
        return dao.selectByCondition(c, null);
    }
	
	protected T selectOne(Condition c) {
        List<T> list = selectList(c);
        return CollectionUtils.isEmpty(list) ? null : list.get(0);
    }

	@Override
    public Page<T> selectPage(Page<T> p) {
        return selectPage(null, p);
    }
    
    protected Page<T> selectPage(Condition c, Page<T> p) {
        dao.selectByCondition(c, p);
        return p;
    }

    @Override
	public T selectById(ID id) {
		return dao.selectById(id);
	}

    @Override
    public List<T> selectByIds(Collection<ID> ids) {
        return selectList(Condition.create().in(DynamicSqlProvider.ID, ids));
    }

    @Override
	public void save(T t) {
		if (t.getId() == null) {
			insert(t);
		}
		else {
			update(t);
		}
	}

    @Override
    @Transactional(readOnly = false)
	public void save(Collection<T> ts) {
		for (T t : ts) {
			save(t);
		}
	}
	
    @Override
	public void insert(T t) {
        dao.insert(t);
    }

    @Override
    @Transactional(readOnly = false)
    public void insert(Collection<T> ts) {
        for (T t : ts) {
            insert(t);
        }
    }

    @Override
	public void update(T t) {
		dao.update(t);
	}

    @Override
    @Transactional(readOnly = false)
	public void update(Collection<T> ts) {
		for (T t : ts) {
			update(t);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void delete(T t) {
		deleteById((ID) t.getId());
	}

	@SuppressWarnings("unchecked")
	@Override
    public void delete(Collection<T> ts) {
        deleteByIds(ConvertUtils.convert(ts, t -> (ID)t.getId()));
	}

	@Override
	public void deleteById(ID id) {
	    deleteByCondition(Condition.create().eq(DynamicSqlProvider.ID, id));
	}

	@Override
	public void deleteByIds(Collection<ID> ids) {
	    deleteByCondition(Condition.create().in(DynamicSqlProvider.ID, ids));
	}
	
	protected void deleteByCondition(Condition condition) {
	    dao.deleteByCondition(condition);
    }
}
