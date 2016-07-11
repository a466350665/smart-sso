package com.smart.job.demo;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 支持分布式，多JVM部署，用数据库锁保持执行唯一性
 * 
 * @author Joe
 */
public class TestJob implements Job {

	private static Logger LOGGER = LoggerFactory.getLogger(TestJob.class);

	@Override
	public void execute(JobExecutionContext context) throws JobExecutionException {
		LOGGER.info("定时任务开始处理");
		// TODO
		LOGGER.info("定时任务结束处理");
	}
}
