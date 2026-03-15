import { test, expect, Page } from '@playwright/test';

async function createNewGame(page: Page, programs: number, processors: number, ados: number) {
    await page.goto('http://localhost:5173/');

    // Click the new game button
    await page.getByRole('button', { name: 'New Game' }).click();

    // The dialog should be visible
    await expect(page.getByRole('dialog', { name: 'Create New Game' })).toBeVisible();

    // Fill in the form
    await page.getByLabel('Number of Programs').fill(programs.toString());
    await page.getByLabel('Number of Processors').fill(processors.toString());
    await page.getByLabel('Number of ADOs per program').fill(ados.toString());

    // Click the create button
    await page.getByRole('button', { name: 'Create' }).click();

    // The game should be visible
    await expect(page.getByText('Ticks:')).toBeVisible();
    await expect(page.getByText('Programs')).toBeVisible();
    await expect(page.getByText('Processors')).toBeVisible();
}

test.describe('App', () => {

  test.describe('New Game Dialog', () => {
    test('should show the new game dialog', async ({ page }) => {
      await page.goto('http://localhost:5173/');

      // The environment is not loaded yet, so we should see the new game button
      await expect(page.getByRole('button', { name: 'New Game' })).toBeVisible();

      // Click the new game button
      await page.getByRole('button', { name: 'New Game' }).click();

      // The dialog should be visible
      await expect(page.getByRole('dialog', { name: 'Create New Game' })).toBeVisible();

      // The dialog should have the correct fields
      await expect(page.getByLabel('Number of Programs')).toBeVisible();
      await expect(page.getByLabel('Number of Processors')).toBeVisible();
      await expect(page.getByLabel('Number of ADOs per program')).toBeVisible();

      // The dialog should have the correct buttons
      await expect(page.getByRole('button', { name: 'Cancel' })).toBeVisible();
      await expect(page.getByRole('button', { name: 'Create' })).toBeVisible();
    });

    test('should create a new game and display it', async ({ page }) => {
      await createNewGame(page, 2, 4, 3);
    });
  });

  test.describe('Game Interaction', () => {
    test.beforeEach(async ({ page }) => {
      await createNewGame(page, 2, 4, 3);
    });

    test('should advance ticks', async ({ page }) => {
      // Get the initial tick count
      const initialTicks = await page.getByText(/Ticks: \d+/).textContent();
      expect(initialTicks).not.toBeNull();
      const initialTickCount = parseInt(initialTicks!.split(' ')[1]);

      // Click the advance ticks button
      await page.getByRole('button', { name: 'Advance Ticks' }).click();

      // The tick count should have increased
      const newTicks = await page.getByText(/Ticks: \d+/).textContent();
      expect(newTicks).not.toBeNull();
      const newTickCount = parseInt(newTicks!.split(' ')[1]);
      expect(newTickCount).toBeGreaterThan(initialTickCount);
    });

    test('should move an ado to a processor', async ({ page }) => {
        // Find an Ado to drag
        const ado = page.locator('.ado').first();
        const adoId = await ado.getAttribute('data-ado-id');
        expect(adoId).not.toBeNull();
      
        // Find a Processor to drop onto
        const processor = page.locator('.processor').first();
        const processorId = await processor.getAttribute('data-processor-id');
        expect(processorId).not.toBeNull();
      
        // Drag and drop the Ado
        await ado.dragTo(processor);
      
        // The Ado should now be inside the Processor
        const processorWithAdo = processor.locator(`[data-ado-id="${adoId}"]`);
        await expect(processorWithAdo).toBeVisible();
      });
  });
});
