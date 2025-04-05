import React, { useState } from 'react';
import { 
  Box, 
  Button, 
  Heading, 
  Input, 
  Stack, 
  Text, 
  Image, 
  Code, 
  Flex 
} from "@chakra-ui/react";
import { TwoFactorService } from "@/client";
import useCustomToast from "../hooks/useCustomToast";

interface TwoFactorSetupProps {
  onComplete?: () => void;
  tempToken?: string;
}

export const TwoFactorSetup: React.FC<TwoFactorSetupProps> = ({ onComplete, tempToken }) => {
  const [step, setStep] = useState<'init' | 'verify'>('init');
  const [qrCode, setQrCode] = useState<string>('');
  const [secret, setSecret] = useState<string>('');
  const [loading, setLoading] = useState<boolean>(false);
  const [password, setPassword] = useState<string>('');
  const [code, setCode] = useState<string>('');
  const [error, setError] = useState<string>('');
  const showToast = useCustomToast();

  const handleEnable = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');
    setLoading(true);
    
    try {
      // If we have a tempToken, we need to set it in the Authorization header
      const headers: Record<string, string> = {};
      if (tempToken) {
        headers['Authorization'] = `Bearer ${tempToken}`;
      }
      
      const response = await TwoFactorService.enable2Fa({
        requestBody: {
          password
        },
        headers
      });
      
      setQrCode(response.qr_code);
      setSecret(response.secret);
      setStep('verify');
      showToast(
        "Success", 
        "Scan the QR code with your authenticator app", 
        "success"
      );
    } catch (error) {
      console.error('Failed to enable 2FA:', error);
      setError('Failed to enable 2FA. Please check your password and try again.');
      showToast(
        "Error", 
        "Failed to enable 2FA. Please check your password and try again.", 
        "error"
      );
    } finally {
      setLoading(false);
    }
  };

  const handleVerify = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');
    setLoading(true);
    
    try {
      // If we have a tempToken, we need to set it in the Authorization header
      const headers: Record<string, string> = {};
      if (tempToken) {
        headers['Authorization'] = `Bearer ${tempToken}`;
      }
      
      await TwoFactorService.verify2Fa({
        requestBody: {
          code
        },
      });
      
      showToast(
        "Success", 
        "Two-factor authentication enabled successfully!", 
        "success"
      );
      if (onComplete) {
        onComplete();
      }
    } catch (error) {
      console.error('Failed to verify 2FA code:', error);
      setError('Invalid verification code. Please try again.');
      showToast(
        "Error", 
        "Invalid verification code. Please try again.", 
        "error"
      );
    } finally {
      setLoading(false);
    }
  };

  return (
    <Box p={4} borderWidth="1px" borderRadius="lg" shadow="md">
      <Box>
        {step === 'init' ? (
          <>
            <Heading as="h3" size="md" mb={4}>Enable Two-Factor Authentication</Heading>
            <Text mb={6}>
              Two-factor authentication adds an extra layer of security to your account.
              After enabling, you'll need to enter a verification code from your authenticator app when logging in.
            </Text>
            
            <Box
              as="form"
              onSubmit={handleEnable}
            >
              <Box mb={4}>
                <Text fontWeight="medium" mb={1}>
                  Confirm your password
                </Text>
                <Input 
                  id="password"
                  type="password"
                  placeholder="Password"
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                  required
                  mt={1}
                />
                {error && (
                  <Text color="red.500" fontSize="sm" mt={1}>{error}</Text>
                )}
              </Box>
              
              <Button 
                type="submit" 
                colorScheme="blue" 
                mt={4}
                loading={loading}
                disabled={loading}
              >
                Enable 2FA
              </Button>
            </Box>
          </>
        ) : (
          <>
            <Heading as="h3" size="md" mb={4}>Scan QR Code</Heading>
            <Text mb={6}>
              Scan this QR code with your authenticator app (like Google Authenticator, Authy, or Microsoft Authenticator).
              After scanning, enter the verification code shown in your app.
            </Text>
            
            <Stack gap={6}>
              <Flex justifyContent="center">
                {qrCode && <Image src={qrCode} alt="QR Code" maxWidth="200px" />}
              </Flex>
              
              <Box>
                <Text fontWeight="bold" mb={2}>
                  If you can't scan the QR code, enter this code manually in your authenticator app:
                </Text>
                <Code p={2}>{secret}</Code>
              </Box>
              
              <Box
                as="form"
                onSubmit={handleVerify}
              >
                <Box mb={4}>
                  <Text fontWeight="medium" mb={1}>
                    Verification Code
                  </Text>
                  <Input 
                    id="code"
                    placeholder="Enter 6-digit code"
                    maxLength={6}
                    value={code}
                    onChange={(e) => setCode(e.target.value)}
                    required
                    mt={1}
                  />
                  {error && (
                    <Text color="red.500" fontSize="sm" mt={1}>{error}</Text>
                  )}
                </Box>
                
                <Button 
                  type="submit" 
                  colorScheme="blue" 
                  mt={4}
                  loading={loading}
                  disabled={loading}
                >
                  Verify and Enable
                </Button>
              </Box>
            </Stack>
          </>
        )}
      </Box>
    </Box>
  );
};

export default TwoFactorSetup;
