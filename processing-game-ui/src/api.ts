const BASE_URL = 'http://localhost:5251/api/game';

export const api = {
    getEnvironment: async () => {
        const response = await fetch(BASE_URL);
        if (!response.ok) {
            throw new Error('Failed to fetch environment');
        }
        return response.json();
    },

    moveAdo: async (adoId: string, processorId: string) => {
        const response = await fetch(`${BASE_URL}/move`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ adoId, processorId }),
        });
        if (!response.ok) {
            throw new Error(await response.text());
        }
        return response.json();
    },

    tick: async (amount: number) => {
        const response = await fetch(`${BASE_URL}/tick`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(amount),
        });
        if (!response.ok) {
            const errorText = await response.text();
            // The text might be a JSON object from the server with more details
            try {
                const errorJson = JSON.parse(errorText);
                throw new Error(errorJson.title || 'Error advancing time');
            } catch {
                throw new Error(errorText || 'Error advancing time');
            }
        }
        return response.json();
    },

    newGame: async (config: { programs: number, ados: number, processors: number }) => {
        const response = await fetch(`${BASE_URL}/new`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(config),
        });
        if (!response.ok) {
            throw new Error('Failed to start a new game');
        }
        return response.json();
    }
};
