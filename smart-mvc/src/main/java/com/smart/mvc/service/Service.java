package com.smart.mvc.service;

import com.smart.mvc.model.PersistentObject;

/**
 * Service接口
 * 
 * @param <T>
 * @author Joe
 */
public interface Service<T extends PersistentObject> extends BaseService<T, Integer> {
}
