package com.smart.mvc.locks;

import java.io.IOException;

public class DistributedLockTest {
	public static void main(String[] args) throws IOException, InterruptedException {
		for (int i = 0; i < 10; i++) {
			new Thread() {
				public void run() {
					try {
						DistributedLock lock = new ZookeeperLock(
								"192.168.33.16:2181,192.168.33.16:2182,192.168.33.16:2183", "jason");
						if (lock.tryLock()) {
							System.out.println(Thread.currentThread().getName() + "在执行");
							Thread.sleep(1000);
						}
						lock.unlock();
					}
					catch (Exception e) {
						e.printStackTrace();
					}
				}
			}.start();
		}
	}
}
