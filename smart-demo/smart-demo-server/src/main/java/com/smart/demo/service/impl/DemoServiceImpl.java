package com.smart.demo.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.smart.demo.dao.DemoDao;
import com.smart.demo.model.Demo;
import com.smart.demo.service.DemoService;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.service.mybatis.impl.ServiceImpl;

@Service("demoService")
public class DemoServiceImpl extends ServiceImpl<DemoDao, Demo, Integer> implements DemoService {

	@Autowired
	public void setDao(DemoDao dao) {
		this.dao = dao;
	}

	@Override
	public Pagination<Demo> findPaginationByName(String name, Pagination<Demo> p) {
		dao.findPaginationByName(name, p);
		return p;
	}
	
	public Demo findByName(String name) {
		return dao.findByName(name);
	}
}
