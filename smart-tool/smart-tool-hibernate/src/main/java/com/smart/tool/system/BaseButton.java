package com.smart.tool.system;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;

import javax.swing.JButton;

/**
 * 按钮基类
 * 
 * @author Joe
 */
public class BaseButton extends JButton {
	private static final long serialVersionUID = 1L;
	public static final Color BUTTON_COLOR1 = new Color(125, 161, 237);
	public static final Color BUTTON_COLOR2 = new Color(91, 118, 173);
	public static final Color BUTTON_BAK_COLOR1_1 = new Color(108, 135, 210, 179);
	public static final Color BUTTON_BAK_COLOR1_2 = new Color(108, 135, 210, 255);
	public static final Color BUTTON_BAK_COLOR2_1 = new Color(180, 230, 250, 179);
	public static final Color BUTTON_BAK_COLOR2_2 = new Color(180, 230, 250, 255);
	public static final Color BUTTON_FOREGROUND_COLOR = Color.BLACK;
	private boolean hover;

	public BaseButton() {
		setBorderPainted(false);
		setFocusPainted(false);
		setContentAreaFilled(false);
		setForeground(BUTTON_FOREGROUND_COLOR);
		addMouseListener(new MouseAdapter() {
			public void mouseEntered(MouseEvent e) {
				BaseButton.this.hover = true;
				BaseButton.this.repaint();
			}

			public void mouseExited(MouseEvent e) {
				BaseButton.this.hover = false;
				BaseButton.this.repaint();
			}
		});
	}

	protected void paintComponent(Graphics g) {
		Graphics2D g2d = (Graphics2D) g.create();
		int h = getHeight();
		int w = getWidth();
		float tran = 1.0F;
		if (!this.hover) {
			tran = 0.7F;
		}
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		GradientPaint p2;
		GradientPaint p1;
		if (getModel().isPressed()) {
			p1 = new GradientPaint(0.0F, 0.0F, new Color(0, 0, 0), 0.0F, h - 1, new Color(100, 100, 100));
			p2 = new GradientPaint(0.0F, 1.0F, new Color(0, 0, 0, 50), 0.0F, h - 3, new Color(255, 255, 255, 100));
		}
		else {
			p1 = new GradientPaint(0.0F, 0.0F, new Color(100, 100, 100), 0.0F, h - 1, new Color(0, 0, 0));
			p2 = new GradientPaint(0.0F, 1.0F, new Color(255, 255, 255, 100), 0.0F, h - 3, new Color(0, 0, 0, 50));
		}

		g2d.setComposite(AlphaComposite.getInstance(3, tran));
		Shape clip = g2d.getClip();

		RoundRectangle2D.Float r2d = new RoundRectangle2D.Float(0.0F, 0.0F, w - 1, h - 1, h, h);
		g2d.clip(r2d);
		GradientPaint gp = new GradientPaint(0.0F, 0.0F, Color.WHITE, 0.0F, h, Color.WHITE, true);
		g2d.setPaint(gp);
		g2d.fillRect(0, 0, w, h);

		if (this.hover) {
			RoundRectangle2D.Float r2d2 = new RoundRectangle2D.Float(0.0F, 0.0F, w - 1, h - 1, 0.0F, 0.0F);
			g2d.clip(r2d2);
			GradientPaint gp2 = new GradientPaint(0.0F, 0.0F, BUTTON_BAK_COLOR1_1, 0.0F, h, BUTTON_BAK_COLOR1_2, true);
			g2d.setPaint(gp2);
			g2d.fillRect(0, 0, w, h);
		}
		g2d.setClip(clip);

		g2d.setPaint(p1);
		g2d.drawRoundRect(0, 0, w - 1, h - 1, h, h);
		g2d.setPaint(p2);
		g2d.drawRoundRect(1, 1, w - 3, h - 3, h - 2, h - 2);
		g2d.dispose();
		super.paintComponent(g);
	}
}