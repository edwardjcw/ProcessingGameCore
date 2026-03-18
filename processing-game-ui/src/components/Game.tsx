import { useState, useEffect } from 'react';
import type { Environment } from '../types';
import { Program } from './Program';
import { Processor } from './Processor';
import { Box, Button, Grid, TextField, Typography } from '@mui/material';
import { api } from '../api';

interface GameProps {
    environment: Environment | null;
    setEnvironment: (environment: Environment) => void;
    fetchEnvironment: () => Promise<void>;
}

export const Game: React.FC<GameProps> = ({ environment, setEnvironment, fetchEnvironment }) => {
    const [tickAmount, setTickAmount] = useState<number>(1);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        console.log('environment updated', environment);
    }, [environment]);

    const handleDrop = async (adoId: string, processorId: string) => {
        try {
            const data = await api.moveAdo(adoId, processorId);
            setEnvironment(data);
        } catch (error: any) {
            alert(`Error moving ado: ${error.message}`);
            fetchEnvironment(); // to get back to consistent state
        }
    };

    const handleTick = async (amount: number = 1) => {
        try {
            const data = await api.tick(amount);
            setEnvironment(data);
        } catch (error: any) {
            alert(`Error advancing time: ${error.message}`);
        }
    };

    if (error) {
        return <Typography color="error">Error: {error}. Please ensure the backend server is running and accessible.</Typography>;
    }

    if (!environment) {
        return <Typography>Loading...</Typography>;
    }

    return (
        <Box sx={{ flexGrow: 1 }}>
            <Box sx={{ display: 'flex', alignItems: 'center', gap: 2, mb: 2 }}>
                <Typography variant="h4">Ticks: {environment.ticks}</Typography>
                <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                    <TextField
                        type="number"
                        value={tickAmount}
                        onChange={(e) => setTickAmount(Math.max(1, parseInt(e.target.value) || 1))}
                        inputProps={{ min: 1 }}
                        label="Ticks"
                        size="small"
                        sx={{ width: '100px' }}
                    />
                    <Button variant="contained" onClick={() => handleTick(tickAmount)}>Advance Ticks</Button>
                </Box>
            </Box>
            <Grid container spacing={2}>
                <Grid size={{ xs: 6 }}>
                    <Typography variant="h5">Programs</Typography>
                    {Object.values(environment.programs).map((program) => (
                        <Program key={program.id} program={program} handleDrop={handleDrop} />
                    ))}
                </Grid>
                <Grid size={{ xs: 6 }}>
                    <Typography variant="h5">Processors</Typography>
                    {Object.values(environment.processors).map((processor) => (
                        <Processor key={processor.id} processor={processor} />
                    ))}
                </Grid>
            </Grid>
        </Box>
    );
};
