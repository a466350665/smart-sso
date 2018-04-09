package com.smart.mvc.locks;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.KeeperException;
import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.Watcher;
import org.apache.zookeeper.ZooDefs;
import org.apache.zookeeper.ZooKeeper;
import org.apache.zookeeper.data.Stat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.mvc.util.StringUtils;

/**
 * 基于Zookeeper设计的分布式锁
 * 
 * @author Joe
 */
public class ZookeeperLock extends DistributedLock {
	private final Logger logger = LoggerFactory.getLogger(getClass());
	private final int SESSION_TIMEOUT = 5000;
	private String root = "/lock-";
	private CountDownLatch countDownLatch = new CountDownLatch(1);

	private ZooKeeper zookeeper;
	private String lockPath;

	public ZookeeperLock(String address, String lockName) {
		if (StringUtils.isBlank(address)) {
			throw new RuntimeException("zookeeper address can not be empty!");
		}
		if (StringUtils.isBlank(lockName)) {
			throw new RuntimeException("lockName can not be empty!");
		}
		zookeeper = connectServer(address);
		if (zookeeper != null) {
			root += lockName;
			try {
				Stat stat = zookeeper.exists(root, false);
				if (stat == null) {
					zookeeper.create(root, new byte[0], ZooDefs.Ids.OPEN_ACL_UNSAFE, CreateMode.PERSISTENT);
				}
			}
			catch (KeeperException e) {
				e.printStackTrace();
			}
			catch (InterruptedException e) {
				e.printStackTrace();
			}

		}
	}

	/**
	 * 获取锁
	 *
	 * @throws KeeperException
	 * @throws InterruptedException
	 */
	public void lock() {
		try {
			lockPath = zookeeper.create(root + "/lock_", new byte[0], ZooDefs.Ids.OPEN_ACL_UNSAFE,
					CreateMode.EPHEMERAL_SEQUENTIAL);
			judgeLock();
		}
		catch (KeeperException e) {
			e.printStackTrace();
		}
		catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	/**
	 * 判断是否能够拥有该锁
	 *
	 * @throws KeeperException
	 * @throws InterruptedException
	 */
	private void judgeLock() throws KeeperException, InterruptedException {
		List<String> list = zookeeper.getChildren(root, false);
		String[] nodes = list.toArray(new String[list.size()]);
		Arrays.sort(nodes);// 从小到大排序
		if (nodes.length > 0) {
			if (!lockPath.equals(root + "/" + nodes[0])) {
				waitForLock(nodes[0]);
			}
			else {
				countDownLatch.countDown();
			}
		}
		else {
			countDownLatch.countDown();
		}
	}

	/**
	 * 等待锁(写法1)
	 *
	 * @param nodePath
	 * @throws InterruptedException
	 * @throws KeeperException
	 */
	private void waitForLock(String nodePath) throws InterruptedException, KeeperException {
		Stat stat = zookeeper.exists(root + "/" + nodePath, false);
		if (stat == null) {
			judgeLock();
		}
		else {
			waitForLock(nodePath);
		}
	}

	/**
	 * 释放锁
	 */
	public void unlock() {
		if (StringUtils.isBlank(lockPath)) {
			logger.error("no need to unlock!");
		}
		try {
			zookeeper.delete(lockPath, -1);
		}
		catch (InterruptedException e) {
			e.printStackTrace();
		}
		catch (KeeperException e) {
			e.printStackTrace();
		}
	}

	/**
	 * 尝试获得锁，能获得就立马获得锁并返回true，如果不能获得就立马返回false
	 *
	 * @return
	 */
	public boolean tryLock() {
		try {
			lockPath = zookeeper.create(root + "/lock_", new byte[0], ZooDefs.Ids.OPEN_ACL_UNSAFE,
					CreateMode.EPHEMERAL_SEQUENTIAL);
			List<String> list = zookeeper.getChildren(root, false);
			String[] nodes = list.toArray(new String[list.size()]);
			Arrays.sort(nodes);// 从小到大排序
			if (lockPath.equals(root + "/" + nodes[0])) {
				return true;
			}
		}
		catch (KeeperException e) {
			e.printStackTrace();
		}
		catch (InterruptedException e) {
			e.printStackTrace();
		}
		return false;
	}

	/**
	 * 连接zookeeper服务器
	 *
	 * @param address
	 * @return
	 */
	private ZooKeeper connectServer(String address) {
		final CountDownLatch latch = new CountDownLatch(1);
		ZooKeeper zk = null;
		try {
			zk = new ZooKeeper(address, SESSION_TIMEOUT, new Watcher() {
				@Override
				public void process(WatchedEvent event) {
					if (event.getState() == Event.KeeperState.SyncConnected) {
						latch.countDown();
					}
				}
			});
			latch.await();
		}
		catch (IOException e) {
			logger.error("IOException", e);
		}
		catch (InterruptedException ex) {
			logger.error("InterruptedException", ex);
		}
		return zk;
	}
}
